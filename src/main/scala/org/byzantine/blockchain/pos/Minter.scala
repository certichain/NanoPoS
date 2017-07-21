package org.byzantine.blockchain.pos
import org.byzantine.blockchain._
import scala.collection.mutable
import java.time.Instant


class Minter(nodeID: Int) extends Node(nodeID, PoSGenesisBlock) {

  override def receive = super.receive orElse {
    case Minter.MintCmd(to) => mint(to)
  }

  def mint(to: Address): Unit = {
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
        log.info("Minted block " + mintedBlock.hash + " containing " + mintedBlock.tx.length + " transactions.\n" + mintedBlock)
        Peers.gossip(BlockMsg(mintedBlock))

        for (tx <- mintedBlock.tx) {
          txPool.remove(tx.hash)
        }
      }
    }
  }
}

object Minter {
  case class MintCmd(to: Address) extends Node.ControlMessage
}

