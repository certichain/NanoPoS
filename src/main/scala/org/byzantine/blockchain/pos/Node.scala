package org.byzantine.blockchain.pos

import java.time.Instant

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import org.byzantine.blockchain._

import scala.collection.mutable
import scala.language.postfixOps

// Sent  by the network
abstract class Message
case class PoSBlockMsg(block: Block[ProofOfStake]) extends Message
case class TransactionMsg(tx: Transaction) extends Message
case class InvMsg(known: Set[Hash]) extends Message
case class GetDataMsg(hash: Hash) extends Message
case class ConnectMsg(peer: ActorRef) extends Message


// For now, we (unreasonably) assume every node talks directly with every other node, so there's no need
// to relay/gossip information you receive
class Node(val nodeID: Int) extends Actor {
  val log = Logging(context.system, this)

  val blockTree = new BlockTree()
  val txPool = new mutable.HashMap[Hash, Transaction]()

  object Peers {
    private val peers = new mutable.HashSet[ActorRef]()
    private val informed = new mutable.HashSet[ActorRef]() // Peers that we've told about ourselves

    def number: Int = peers.size
    def names: List[String] = peers.map(p => p.toString).toList

    def addPeer(peer: ActorRef): Unit = {
      peers += peer

      if (!informed.contains(peer)) {
        peer ! ConnectMsg(self)
        informed += peer
      }
    }

    def gossip(message: Message, peers: Set[ActorRef] = peers.toSet): Unit = {
      for (peer <- peers) {
        peer ! message
      }
    }

  }

//  def gossip(message: Message, except: ActorRef): Unit =  gossip(message, peers.filter(x => x != except))

  def chain = blockTree.chain
  def knownHashes: Set[Hash] = blockTree.knownBlockHashes ++ txPool.keys.toSet

  def receive = {
    // Network messages
    case PoSBlockMsg(block) => extend(block); Peers.gossip(InvMsg(knownHashes))
    case TransactionMsg(tx) => txPool.put(tx.hash, tx); Peers.gossip(InvMsg(knownHashes))
    case ConnectMsg(peer) => Peers.addPeer(peer)

    case InvMsg(peerHashes) => {
      val unknown: Set[Hash] = peerHashes -- knownHashes
      for (hash <- unknown) {
        sender() ! GetDataMsg(hash)
      }
    }

    case GetDataMsg(hash) => {
      txPool.get(hash) match {
        case Some(tx) => sender() ! TransactionMsg(tx)
        case None =>
      }

      blockTree.get(hash) match {
        case Some(block) => sender() ! PoSBlockMsg(block)
        case None =>
      }
    }

    // Node control commands – sent exclusively by supervisor, not other nodes
    case Node.TransferCmd(from, to, amount) => transfer(from, to, amount)
    case Node.MintCmd(to) => mint(to)
    case Node.MemPoolCmd => sender() ! txPool.toSet
    case Node.BlockchainCmd => sender() ! chain.top.hash
    case Node.ListPeersCmd => log.info(Peers.number + " known peers: " + Peers.names)

    case _ =>
  }

  def extend(block: Block[ProofOfStake]): Unit = {
    if (!blockTree.has(block) && blockTree.extensionPossibleWith(block)) {
      blockTree.extend(block)

      // Remove from txPool transactions that were included in this block
      for (tx <- block.tx.filter(t => txPool.contains(t.hash))) {
        txPool.remove(tx.hash)
      }
    }
  }

  def transfer(from: Address, to: Address, amount: Int): Unit = {
    val canSend = from == Address(nodeID) && chain.state.balance(from) >= amount
    if (canSend) {
      log.info(s"Sending $amount from $from to $to.")
      val tx = new Transaction(from, to, amount)
      val msg = TransactionMsg(tx)

      txPool.put(tx.hash, tx)
//      Peers.gossip(msg)
      Peers.gossip(InvMsg(knownHashes))
    }
    sender() ! canSend
  }

  def mint(to: Address): Unit = {
    // Transactions that are acceptable on top of blockTree's current chain
    def acceptableTransactions(txList: List[Transaction], timestamp: Long, validPOS: ProofOfStake): List[Transaction] = {
      val acceptedTransactions = new mutable.ListBuffer[Transaction]()

      for (tx <- txList) {
        val candidateBlock = new Block(blockTree.top.hash, acceptedTransactions.toList ++ List(tx), timestamp, validPOS)
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
        log.info("Minted block " + mintedBlock.hash + " containing " + mintedBlock.tx.length + " transactions.\n" + mintedBlock)
        Peers.gossip(PoSBlockMsg(mintedBlock))

        for (tx <- mintedBlock.tx) {
          txPool.remove(tx.hash)
        }
      }
    }
  }
}

object Node {
  // Sent locally to control the actor
  abstract class ControlMessage extends Message
  case class TransferCmd(from: Address, to: Address, amount: Int) extends ControlMessage
  case class MintCmd(to: Address) extends ControlMessage
  case class MemPoolCmd() extends ControlMessage
  case class BlockchainCmd() extends ControlMessage
  case class ListPeersCmd() extends ControlMessage

}