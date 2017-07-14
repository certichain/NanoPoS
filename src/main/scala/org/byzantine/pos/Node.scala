package org.byzantine.pos

import java.time.Instant

import akka.actor.{Actor, ActorRef}
import akka.event.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps


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

  def chain() = blockTree.chain

  def receive = {
    case BlockMsg(block) => extend(block)
    case TransactionMsg(tx) => txPool.put(tx.hash, tx)

    case Node.AddPeerMsg(peer) => addPeer(peer)
    case Node.TransferMsg(from, to, amount) => transfer(from, to, amount)
    case Node.MintMsg(to) => mint(to)
    case Node.MemPoolMsg => log.info("Mempool of size " + txPool.size + " => " + txPool.toList.toString())
    case Node.BlockchainMsg => log.info("Block height: " + blockTree.chain.blocks.length + "\n" + blockTree.chain.toString)

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
    val canSend = from == Address(nodeID) && chain.state.balance(from) >= amount
    if (canSend) {
      log.info(s"Sending $amount from $from to $to.")
      val tx = new Transaction(from, to, amount)
      val msg = new TransactionMsg(tx)

      txPool.put(tx.hash, tx)
      sendToAllPeers(msg)
    }
    sender() ! canSend
  }

  def mint(to: Address): Unit = {
    // Transactions that are acceptable on top of blockTree's current chain
    def acceptableTransactions(txList: List[Transaction], timestamp: Long, validPOS: ProofOfStake): List[Transaction] = {
      val acceptedTransactions = new ListBuffer[Transaction]()

      for (tx <- txList) {
        val candidateBlock = new Block(blockTree.top.hash, acceptedTransactions.toList ++ List(tx), timestamp, validPOS)
        if (blockTree.extensionPossibleWith(candidateBlock)) {
          acceptedTransactions += tx
        }
      }

      acceptedTransactions.toList
    }

    val currentTimestamp = Instant.now.getEpochSecond
    val pos = new ProofOfStake(currentTimestamp, chain.consensus.POS.stakeModifier, Address(nodeID))
    val okTransactions = acceptableTransactions(txPool.values.toList, currentTimestamp, pos)

    if (okTransactions.nonEmpty && chain.consensus.POS.stake(Address(nodeID)) != 0 && chain.consensus.POS.validate(pos)) {
      val mintedBlock = new Block(blockTree.top.hash, new Coinbase(to) :: okTransactions, currentTimestamp, pos)

      if (blockTree.extensionPossibleWith(mintedBlock)) {
        extend(mintedBlock)
        log.info("Minted block " + mintedBlock.hash + " containing " + mintedBlock.tx.length + " transactions.\n" + mintedBlock)
        sendToAllPeers(new BlockMsg(mintedBlock))

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
  case class AddPeerMsg(peer: ActorRef) extends ControlMessage
  case class TransferMsg(from: Address, to: Address, amount: Int) extends ControlMessage
  case class MintMsg(to: Address) extends ControlMessage
  case class MemPoolMsg() extends ControlMessage
  case class BlockchainMsg() extends ControlMessage
}