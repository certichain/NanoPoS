package org.byzantine.pos

import scala.collection.mutable
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.{Logging}
import scala.collection.mutable.ListBuffer
import language.postfixOps
import scala.concurrent.duration._


// Sent  by the network
abstract class Message
case class BlockMsg(block: Block) extends Message
case class TransactionMsg(tx: Transaction) extends Message

// For now, we (unreasonably) assume every node talks directly with every other node, so there's no need
// to relay/gossip information you receive
class Node extends Actor {
  val nodeID = self.path.name
  val blockTree = new BlockTree()
  val txPool = new mutable.HashMap[Hash, Transaction]()

  val peers = new ListBuffer[ActorRef]

  def peerList = peers.toList

  val log = Logging(context.system, this)

  def chain = blockTree.chain

  def receive = {
    case BlockMsg(block) => blockTree.extend(block)
    case TransactionMsg(tx) => txPool.put(new Hash(tx), tx)

    case Node.AddPeerMsg(peer) => addPeer(peer)
    case Node.TransferMsg(from, to, amount) => transfer(from, to, amount)
    case Node.MintMsg(to) => mint(to)

    case _ =>
  }

  def addPeer(peer: ActorRef): Unit = peers.append(peer)

  private def sendToAllPeers(message: Message): Unit = {
    for (peer <- peerList) {
      peer ! message
    }
  }

  def transfer(from: Address, to: Address, amount: Int): Unit = {
    val tx = new Transaction(from, to, amount)
    val msg = new TransactionMsg(tx)
    sendToAllPeers(msg)
  }

  def mint(to: Address): Unit = {
    if (txPool.nonEmpty) {
      val txList = new Coinbase(to) :: txPool.values.toList
      val mintedBlock = new Block(blockTree.top.hash, txList)

      blockTree.extend(mintedBlock)
      sendToAllPeers(new BlockMsg(mintedBlock))

      txPool.clear()
    }
  }
}

object Node {
  // Sent locally to control the actor
  abstract class ControlMessage extends Message
  case class AddPeerMsg(peer: ActorRef) extends ControlMessage
  case class TransferMsg(from: Address, to: Address, amount: Int) extends ControlMessage
  case class MintMsg(to: Address) extends ControlMessage

}