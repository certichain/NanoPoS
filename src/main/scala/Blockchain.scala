package org.byzantine.pos

import com.roundeights.hasher.Implicits._
import akka.actor. {Actor, ActorSystem}
import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Const {
  val CoinbaseSourceAddress: Address = new Address(-1)
  val CoinbaseAmount: Int = 25
  val GenesisPrevHash: Hash = new Hash("Genesis")
  val GenesisCoinbase: Coinbase = new Coinbase(new Address(0))
  val GenesisTimestamp: Long = 1499774400
}

class Hash(of: String) {
  val hashValue: String = of.sha1

  def this(of: Any) = this(of.toString)

  override def toString: String = hashValue

  override def equals(o: Any) = o match {
    case that: Hash => that.hashValue == this.hashValue
    case _ => false
  }
  override def hashCode = hashValue.hashCode
}
class Address(val addr: Int) {
  override def toString: String = addr.toString
}

class Transaction(val from: Address, val to: Address, val amount: Int, val timestamp: Long) {
  def this(from: Address, to: Address, amount: Int) = this(from, to, amount, Instant.now.getEpochSecond)
  override def toString: String = s"Tx($from, $to, $amount, $timestamp)"
}
class Coinbase(to: Address) extends Transaction(Const.CoinbaseSourceAddress, to, Const.CoinbaseAmount) {
  override def toString: String = s"Coinbase($to, $amount)"
}

class Block(val prevBlockHash: Hash, val tx: List[Transaction], val timestamp: Long) {
  def this(prevBlockHash: Hash, tx: List[Transaction]) = this(prevBlockHash, tx, Instant.now.getEpochSecond)
  override def toString: String = s"Block($prevBlockHash, $timestamp, [" + tx.mkString(", ") + s"])"
}
object GenesisBlock extends Block(Const.GenesisPrevHash, List(Const.GenesisCoinbase), Const.GenesisTimestamp)

class Blockchain(val blocks: List[Block]) {
  override def toString: String = "Blockchain[" + blocks.mkString(", ") + "]"

  def chainForkCompare(that: Blockchain): Int = {
    val lengthCmp = (this.blocks.length - that.blocks.length) match {
      case diff if diff < 0 => -1
      case diff if diff == 0 => 0
      case diff if diff > 0 => 1
    }

    return lengthCmp
  }
}

class BlockTree {
  private val blocks = new mutable.HashMap[Hash, Block]()
  var topHash = this add GenesisBlock

  def top: Block = {
    this get topHash match {
      case Some(x) => x
      case None => throw new Exception("Our blockchain has no top!")
    }
  }

  def chain: Blockchain = this getChainFrom topHash

  def extend(block: Block): Unit = {
    assert(havePrevOf(block), "Trying to extend a block we don't have!")

    val currentChain = chain
    val candidateChain = this getChainFrom block

    // Update topHash according to the ChainForkRule
    topHash = candidateChain.chainForkCompare(currentChain) match {
      case 1 => this add block
      case _ => topHash
    }

  }

  private def add(block: Block): Hash = {
    val hash = new Hash(block)
    blocks put (hash, block)
    return hash
  }

  private def get(hash: Hash): Option[Block] = blocks get hash
  private def getOrError(hash: Hash): Block = {
    (this get hash) match {
      case Some(x) => x
      case None => throw new Exception(s"We don't have block with hash $hash!")
    }
  }

  private def havePrevOf(block: Block): Boolean = {
    (this get block.prevBlockHash) match {
      case Some(x) => true
      case None => false
    }
  }
  private def prevOf(block: Block): Option[Block] = this get block.prevBlockHash
  private def prevOfOrError(block: Block): Block = this getOrError block.prevBlockHash

  private def getChainFrom(hash: Hash): Blockchain = {
    return getChainFrom(this getOrError hash)
  }
  private def getChainFrom(block: Block): Blockchain = {
    var blocks = mutable.ListBuffer[Block]()
    blocks.prepend(block)

    var currentBlock = block
    while(this havePrevOf currentBlock) {
      currentBlock = this prevOfOrError currentBlock
      blocks.prepend(currentBlock)
    }
    assert(currentBlock == GenesisBlock, "Got a chain that doesn't start with the GenesisBlock!")
    
    return new Blockchain(blocks.toList)
  }
}

object Akka extends App {
  val system: ActorSystem = ActorSystem("pos")

  try {

  } finally {
    system.terminate()
  }
}
