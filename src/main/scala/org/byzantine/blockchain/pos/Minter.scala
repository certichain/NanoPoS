package org.byzantine.blockchain.pos
import org.byzantine.blockchain._
import scala.collection.mutable
import java.time.Instant
import akka.actor.{Actor, ActorRef}

case class MintCmd(to: Address) extends ControlMessage

trait MinterRole[Ref] extends NodeRole[Ref, ProofOfStake] {}

trait MinterRoleImpl[Ref] extends NodeRoleImpl[Ref, ProofOfStake] with MinterRole[Ref] {
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

class AkkaMinter(nodeID: Int) extends AkkaNode[ProofOfStake](nodeID, PoSGenesisBlock) with MinterRoleImpl[ActorRef] {
  override def receive: Receive = super.receive orElse {
    case MintCmd(to) => {
      val sentMsgs: ToSend = mint(to)
      sentMsgs.foreach {  case (a, m) => a ! m  }
      if (sentMsgs.nonEmpty) {
        log.info(s"Minted new block!")
      }
    }
    case m => log.info("Received unknown message " + m)
  }
}
