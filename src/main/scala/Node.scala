package org.byzantine.pos

import java.time.Instant
import akka.actor. {Actor, ActorRef}
import akka.event. {Logging}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import language.postfixOps
import scala.concurrent.duration._


// Sent  by the network
abstract class Message
case class BlockMsg(block: Block) extends Message
case class TransactionMsg(tx: Transaction) extends Message

// For now, we (unreasonably) assume every node talks directly with every other node, so there's no need
// to relay/gossip information you receive
class Node(val nodeID: Int) extends Actor {
  val blockTree = new BlockTree()
  val txPool = new mutable.HashMap[Hash, Transaction]()

  val peers = new ListBuffer[ActorRef]

  def peerList = peers.toList

  val log = Logging(context.system, this)

  def chain = blockTree.chain

  def receive = {
    case BlockMsg(block) => extend(block)
    case TransactionMsg(tx) => txPool.put(tx.hash, tx)

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

  private def extend(block: Block): Unit = {
    if (blockTree.extensionPossibleWith(block)) {
      blockTree.extend(block)

      // Remove from txPool transactions that were included in this block
      for (tx <- block.tx.filter(t => txPool.contains(t.hash))) {
        txPool.remove(tx.hash)
      }
    }
  }

  def transfer(from: Address, to: Address, amount: Int): Unit = {
    if (from == Address(nodeID) && chain.state.balance(from) >= amount) {
      log.info(s"Sending $amount from $from to $to.")
      val tx = new Transaction(from, to, amount)
      val msg = new TransactionMsg(tx)

      txPool.put(tx.hash, tx)
      sendToAllPeers(msg)
    }
  }

  def mint(to: Address): Unit = {
    // TODO: make sure you only build valid blocks
    val currentTimestamp = Instant.now.getEpochSecond
    val pos = new ProofOfStake(currentTimestamp, chain.consensus.POS.stakeModifier, Address(nodeID))

    if (chain.consensus.POS.stake(Address(nodeID)) != 0 && chain.consensus.POS.validate(pos)) {
      val txList = new Coinbase(to) :: txPool.values.toList
      val mintedBlock = new Block(blockTree.top.hash, txList, currentTimestamp, pos)

      if (blockTree.extensionPossibleWith(mintedBlock)) {
        extend(mintedBlock)
        log.info("Minted block " + mintedBlock.hash + " containing " + mintedBlock.tx.length + " transactions.\n" + mintedBlock)
        sendToAllPeers(new BlockMsg(mintedBlock))
        txPool.clear()
      }
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