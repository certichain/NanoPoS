package org.byzantine.blockchain

import akka.actor.{Actor, ActorRef}
import akka.event.Logging

import scala.collection.mutable
import scala.language.postfixOps

// Sent  by the network
abstract class Message
case class BlockMsg[P](block: Block[P]) extends Message
case class TransactionMsg(tx: Transaction) extends Message
case class InvMsg(known: Set[Hash]) extends Message
case class GetDataMsg(hash: Hash) extends Message
case class ConnectMsg(peer: ActorRef) extends Message

class Node[P](val nodeID: Int, val genesisBlock: GenesisBlock[P]) extends Actor {
  val log = Logging(context.system, this)

  val blockTree = new BlockTree(genesisBlock)
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

  def chain = blockTree.chain
  def knownHashes: Set[Hash] = blockTree.knownBlockHashes ++ txPool.keys.toSet

  def receive = {
    // Network messages
    case BlockMsg(block) => extend(block.asInstanceOf[Block[P]]); Peers.gossip(InvMsg(knownHashes))
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
        case Some(block) => sender() ! BlockMsg(block)
        case None =>
      }
    }

    // Node control commands – sent exclusively by supervisor, not other nodes
    case Node.TransferCmd(from, to, amount) => transfer(from, to, amount)
    case Node.MemPoolCmd => sender() ! txPool.toSet
    case Node.BlockchainCmd => sender() ! chain.top.hash
    case Node.ListPeersCmd => log.info(Peers.number + " known peers: " + Peers.names)
  }

  def extend(block: Block[P]): Unit = {
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
}

object Node {
  // Sent locally to control the actor
  abstract class ControlMessage extends Message
  case class TransferCmd(from: Address, to: Address, amount: Int) extends ControlMessage
  case class MemPoolCmd() extends ControlMessage
  case class BlockchainCmd() extends ControlMessage
  case class ListPeersCmd() extends ControlMessage
}