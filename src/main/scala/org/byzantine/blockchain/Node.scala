package org.byzantine.blockchain

import akka.actor.{Actor, ActorRef}
import akka.event.Logging

import scala.collection.mutable
import scala.language.postfixOps

abstract class Message
case class BlockMsg[P](block: Block[P]) extends Message
case class TransactionMsg(tx: Transaction) extends Message
case class InvMsg[Ref](sender: Ref, known: Set[Hash]) extends Message
case class GetDataMsg[Ref](requester: Ref, hash: Hash) extends Message
case class ConnectMsg[Ref](peer: Ref) extends Message

abstract class ControlMessage extends Message
case class TransferCmd(from: Address, to: Address, amount: Int) extends ControlMessage
case class MemPoolCmd() extends ControlMessage
case class BlockchainCmd() extends ControlMessage
case class ListPeersCmd() extends ControlMessage


trait NodeRole[Ref, P] {
  val nodeID: Int
  val genesisBlock: GenesisBlock[P]

  type ToSend = Seq[(Ref, Message)]
  type Step = PartialFunction[Any, ToSend]

  protected val self: Ref

  def step: Step

  protected def emitOne(a: Ref, msg: Message) = Seq((a, msg))

  protected def emitMany(a: Ref, msgs: Seq[Message]): ToSend = msgs.map(msg => (a, msg))

  protected def emitMany(as: Seq[Ref], f: Ref => Message): ToSend = as.zip(as.map(a => f(a)))

  protected def emitZero: ToSend = Seq.empty
}

trait NodeRoleImpl[Ref, P] extends NodeRole[Ref, P] {
  protected val blockTree = new BlockTree(genesisBlock)
  protected val txPool = new mutable.HashMap[Hash, Transaction]()

  def chain: Blockchain[P] = blockTree.chain

  def knownHashes: Set[Hash] = blockTree.knownBlockHashes ++ txPool.keys.toSet

  protected object Peers {
    private val peers = new mutable.HashSet[Ref]()
    private val informed = new mutable.HashSet[Ref]() // Peers that we've told about ourselves

    def number: Int = peers.size
    def names: List[String] = peers.map(p => p.toString).toList

    def addPeer(peer: Ref): ToSend = {
      peers += peer

      if (!informed.contains(peer)) {
        informed += peer
        emitOne(peer, ConnectMsg(self))
      } else {
        emitZero
      }
    }

    def gossip(message: Message, peers: Set[Ref] = peers.toSet): ToSend = {
      emitMany(peers.toSeq, _ => message)
    }
  }

  def step: Step = {
    case bm : BlockMsg[P] => extend(bm.block); Peers.gossip(InvMsg(self, knownHashes))
    case TransactionMsg(tx) => txPool.put(tx.hash, tx); Peers.gossip(InvMsg(self, knownHashes))
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

  protected def extend(block: Block[P]): Unit = {
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
}

class AkkaNode[P](val nodeID: Int, val genesisBlock: GenesisBlock[P]) extends NodeRoleImpl[ActorRef, P] with Actor {
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
    case MemPoolCmd => sender() ! txPool.toSet
    case BlockchainCmd => sender() ! chain.top.hash
    case ListPeersCmd => log.info(Peers.number + " known peers: " + Peers.names)
  }
}
