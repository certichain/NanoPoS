package org.byzantine.blockchain

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import org.byzantine.blockchain.pos.{POSHelper, PoSGenesisBlock, ProofOfStake}

import scala.collection.mutable
import scala.language.postfixOps
import java.time.Instant

import scala.util.Random


abstract class Message
case class BlockMsg(block: Block[ProofOfStake]) extends Message
case class TransactionMsg(tx: Transaction) extends Message
case class InvMsg[Ref](sender: Ref, known: Set[Hash]) extends Message
case class GetDataMsg[Ref](requester: Ref, hash: Hash) extends Message
case class AddrMsg[Ref](of: Ref, peers: Set[Ref]) extends Message
case class ConnectMsg[Ref](peer: Ref) extends Message

case class InternalTransition[Ref](info: String) extends Message

abstract class ControlMessage extends Message
// These should be modelled as internal transitions
case class TransferCmd(from: Address, to: Address, amount: Int) extends ControlMessage
case class MintCmd(to: Address) extends ControlMessage

// These don't need to be modelled
case class MemPoolCmd() extends ControlMessage
case class BlockchainCmd() extends ControlMessage
case class ListPeersCmd() extends ControlMessage

trait Communication[Ref] {
  type Transmission = (Ref, Message)  // Ref = destination
  type ToSend = Seq[Transmission]
  type Step = PartialFunction[Any, ToSend]

  protected def emitOne(a: Ref, msg: Message): ToSend = Seq((a, msg))

  protected def emitMany(a: Ref, msgs: Seq[Message]): ToSend = msgs.map(msg => (a, msg))

  protected def emitMany(as: Seq[Ref], f: Ref => Message): ToSend = as.zip(as.map(a => f(a)))

  protected def emitZero: ToSend = Seq.empty
}

trait NodeRole[Ref] extends Communication[Ref] {
  protected val self: Ref
  def step: Step
}

trait NodeImpl[Ref] extends NodeRole[Ref] {
  val nodeID: Int
  val genesisBlock: GenesisBlock[ProofOfStake]

  protected val blockTree = new BlockTree(genesisBlock)
  protected val txPool = new mutable.HashMap[Hash, Transaction]()

  def chain: Blockchain[ProofOfStake] = blockTree.chain

  def knownHashes: Set[Hash] = blockTree.knownBlockHashes ++ txPool.keys.toSet

  protected object Peers {
    private val peers = new mutable.HashSet[Ref]()
    private val informed = new mutable.HashSet[Ref]() // Peers that we've told about ourselves

    def number: Int = peers.size
    def names: List[String] = peers.map(p => p.toString).toList

    def knownPeers: Set[Ref] = peers.toSet

    def addPeer(peer: Ref): ToSend = {
      peers += peer

      if (!informed.contains(peer)) {
        informed += peer
        emitOne(peer, ConnectMsg(self))
      } else {
        emitZero
      }
    }

    def gossip(message: Message, peers: Set[Ref] = knownPeers - self): ToSend = {
      emitMany(peers.toSeq, _ => message)
    }
  }

  def step: Step = {
    case BlockMsg(block) => {
      extend(block)
      // Advertise what you know & who you know
      Peers.gossip(InvMsg(self, knownHashes)) ++ Peers.gossip(AddrMsg(self, Peers.knownPeers))
    }
    case TransactionMsg(tx) => txPool.put(tx.hash, tx); Peers.gossip(InvMsg(self, knownHashes))

    case am: AddrMsg[Ref] => {
      val unknown: Seq[Ref] = (am.peers -- Peers.knownPeers).toSeq
      unknown.flatMap(peer => Peers.addPeer(peer))
    }
    case cm : ConnectMsg[Ref] => Peers.addPeer(cm.peer)

    case im: InvMsg[Ref] =>
      val unknown: Set[Hash] = im.known -- knownHashes
      val messages = unknown.map(hash => GetDataMsg(self, hash)).toSeq
      emitMany(im.sender, messages)

    case gm: GetDataMsg[Ref] =>
      txPool.get(gm.hash) match {
        case Some(tx) => emitOne(gm.requester, TransactionMsg(tx))
        case None => emitZero
      }

      blockTree.get(gm.hash) match {
        case Some(block) => emitOne(gm.requester, BlockMsg(block))
        case None => emitZero
      }
  }

  protected def extend(block: Block[ProofOfStake]): Unit = {
    if (!blockTree.has(block) && blockTree.extensionPossibleWith(block)) {
      blockTree.extend(block)

      // Remove from txPool transactions that were included in this block
      for (tx <- block.tx.filter(t => txPool.contains(t.hash))) {
        txPool.remove(tx.hash)
      }
    }
  }

  def transfer(from: Address, to: Address, amount: Int): ToSend = {
    val canSend = from == Address(nodeID) && chain.state.balance(from) >= amount
    if (canSend) {
      val tx = new Transaction(from, to, amount)
      val msg = TransactionMsg(tx)

      txPool.put(tx.hash, tx)
      Peers.gossip(InvMsg(self, knownHashes))
    } else {
      emitZero
    }
  }

  def mint(to: Address): ToSend = {
    // Transactions that are acceptable on top of blockTree's current chain
    def acceptableTransactions(txList: List[Transaction], timestamp: Long, validProof: ProofOfStake): List[Transaction] = {
      val acceptedTransactions = new mutable.ListBuffer[Transaction]()

      for (tx <- txList) {
        val candidateBlock = new Block(blockTree.top.hash, acceptedTransactions.toList ++ List(tx), timestamp, validProof)
        if (blockTree.extensionPossibleWith(candidateBlock)) {
          acceptedTransactions += tx
        }
      }

      acceptedTransactions.toList
    }

    val currentTimestamp = Instant.now.getEpochSecond
    val posHelper = POSHelper(chain)

    val pos = ProofOfStake(currentTimestamp, posHelper.stakeModifier, Address(nodeID))
    val okTransactions = acceptableTransactions(txPool.values.toList, currentTimestamp, pos)

    if (okTransactions.nonEmpty && posHelper.stake(Address(nodeID)) != 0 && posHelper.validate(pos)) {
      val mintedBlock = new Block(blockTree.top.hash, new Coinbase(to) :: okTransactions, currentTimestamp, pos)

      if (blockTree.extensionPossibleWith(mintedBlock)) {
        extend(mintedBlock)
        for (tx <- mintedBlock.tx) {
          txPool.remove(tx.hash)
        }
        Peers.gossip(BlockMsg(mintedBlock))
      } else {
        emitZero
      }
    } else {
      emitZero
    }
  }
}

class AkkaNode(val nodeID: Int, val genesisBlock: GenesisBlock[ProofOfStake]) extends NodeImpl[ActorRef] with Actor {
  val log = Logging(context.system, this)

  override def receive: Receive = {
    case msg if step.isDefinedAt(msg) => step(msg).foreach { case (a, m) => a ! m }

    // Node control commands – sent exclusively by supervisor, not other nodes
    case TransferCmd(from, to, amount) => {
      val sentMsgs: ToSend = transfer(from, to, amount)
      sentMsgs.foreach {  case (a, m) => a ! m  }

      if (sentMsgs.nonEmpty) {
        log.info(s"Sent $to from addr $from to addr $to!")
      }
      sender() ! sentMsgs.nonEmpty
    }

    case MintCmd(to) => {
      val sentMsgs: ToSend = mint(to)
      sentMsgs.foreach {  case (a, m) => a ! m  }
      if (sentMsgs.nonEmpty) {
        log.info(s"Minted new block!")
      }
    }

    case MemPoolCmd => sender() ! txPool.toSet
    case BlockchainCmd => sender() ! chain.top.hash
    case ListPeersCmd => log.info(Peers.number + " known peers: " + Peers.names)

    case m => log.info("Received unknown message " + m)
  }
}

class SimNode(val nodeID: Int, val genesisBlock: GenesisBlock[ProofOfStake]) extends SimProcess(nodeID) with NodeImpl[SimRef] {
  def init(otherProcesses: Set[SimRef]): ToSend = {
    otherProcesses.flatMap(peer => Peers.addPeer(peer)).toSeq
  }

  override val transitions: Seq[InternalTransition] = Seq(
    // Mint
    _ => {
      if (new POSHelper(chain).stake(Address(nodeID)) != 0) {
        val outbound = mint(Address(nodeID))
        val minted = if (outbound.nonEmpty) emitOne(self, InternalTransition(s"$nodeID minted a block!")) else emitZero
        minted ++ outbound
      }
      else
        emitZero
    },

    // Occasionally transfer money to a peer
    _ => {
      val transferProbability = 0.05
      val amount = 5

      if (chain.state.balance(Address(nodeID)) > amount && (Random.nextDouble() < transferProbability)) {
        val peers = Peers.knownPeers
        val randomPeer = peers.toList(Random.nextInt(peers.size))

        emitOne(self, InternalTransition(s"Send $amount from $nodeID to " + randomPeer.id)) ++
        transfer(Address(nodeID), Address(randomPeer.id), amount)
      } else
        emitZero
    }
  )

}
