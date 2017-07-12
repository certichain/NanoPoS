package org.byzantine.pos

import com.roundeights.hasher.Implicits._
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

  override def equals(o: Any) = o match {
    case that: Address => that.addr == this.addr
    case _ => false
  }

  override def hashCode = addr.hashCode
}

class Transaction(val from: Address, val to: Address, val amount: Int, val timestamp: Long) {
  require(amount >= 0, "Cannot send negative amounts.")

  def this(from: Address, to: Address, amount: Int) = this(from, to, amount, Instant.now.getEpochSecond)

  def hash = new Hash(this)

  override def toString: String = s"Tx($from, $to, $amount, $timestamp)"
}

class Coinbase(to: Address) extends Transaction(Const.CoinbaseSourceAddress, to, Const.CoinbaseAmount) {
  override def toString: String = s"Coinbase($to, $amount)"
}

class Block(val prevBlockHash: Hash, val tx: List[Transaction], val timestamp: Long) {
  require(haveAtMostOneCoinbase, "Blocks must have no more than one coinbase transaction.")
  require(coinbaseHasCorrectAmount, "Blocks cannot contain malformed coinbase transactions.")

  def this(prevBlockHash: Hash, tx: List[Transaction]) = this(prevBlockHash, tx, Instant.now.getEpochSecond)

  override def toString: String = s"Block($prevBlockHash, $timestamp, [" + tx.mkString(", ") + s"])"

  def hash = new Hash(this)

  private def haveAtMostOneCoinbase = tx.filter(t => t.from == Const.CoinbaseSourceAddress).length <= 1

  private def coinbaseHasCorrectAmount = !tx.exists(t => t.from == Const.CoinbaseSourceAddress && t.amount != Const.CoinbaseAmount)
}

object GenesisBlock extends Block(Const.GenesisPrevHash, List(Const.GenesisCoinbase), Const.GenesisTimestamp)

class Blockchain(val blocks: List[Block]) {
  require(blocks.length > 0, "Blockchains must have length > 0.")
  require(blocks(0) == GenesisBlock, "Blockchains must start at the genesis block.")

  override def toString: String = "Blockchain[" + blocks.mkString(", ") + "]"
  def top: Block = blocks.last

  def chainForkCompare(that: Blockchain): Int = {
    val lengthCmp = (this.blocks.length - that.blocks.length) match {
      case diff if diff < 0 => -1
      case diff if diff == 0 => 0
      case diff if diff > 0 => 1
    }

    return lengthCmp
  }

  object State {
    private val balanceSheet = new mutable.HashMap[Address, Int]()
    for (block <- blocks) { processBlock(block) }

    def balance(of: Address): Int = balanceSheet.getOrElse(of, 0)
    private def setBalance(of: Address, amount: Int): Unit = balanceSheet.put(of, amount)
    private def +=(of: Address, diff: Int) = setBalance(of, balance(of) + diff)
    private def -=(of: Address, diff: Int) = setBalance(of, balance(of) - diff)

    private def processBlock(block: Block): Unit = {
      // To simply the logic: can always transfer CoinbaseAmount from CoinbaseSource
      setBalance(Const.CoinbaseSourceAddress, Const.CoinbaseAmount)

      for (tx <- block.tx) {
        processTransaction(tx)
      }
    }

    private def processTransaction(tx: Transaction): Unit = {
      require(balance(tx.from) >= tx.amount, s"Sender cannot send more than they have.")

      -=(tx.from, tx.amount)
      +=(tx.to, tx.amount)
    }
  }

}

class BlockTree {
  private val blocks = new mutable.HashMap[Hash, Block]()
  private var topHash = this add GenesisBlock

  def top: Block = {
    this get topHash match {
      case Some(x) => x
      case None => throw new Exception("Our blockchain has no top!")
    }
  }

  def chain: Blockchain = this getChainFrom topHash

  def extend(block: Block): Unit = {
    assert(havePrevOf(block), "Trying to extend a block we don't have!")

    // Only process nodes we don't have already
    if (!have(block)) {
      val currentChain = chain
      val candidateChain = this getChainFrom block

      // Update topHash according to the ChainForkRule
      topHash = candidateChain.chainForkCompare(currentChain) match {
        case 1 => this add block
        case _ => topHash
      }
    }

  }

  private def add(block: Block): Hash = {
    blocks put(block.hash, block)
    return block.hash
  }

  private def get(hash: Hash): Option[Block] = blocks get hash

  private def getOrError(hash: Hash): Block = {
    (this get hash) match {
      case Some(x) => x
      case None => throw new Exception(s"We don't have block with hash $hash!")
    }
  }

  private def have(hash: Hash): Boolean = {
    (this get hash) match {
      case Some(x) => true
      case None => false
    }
  }

  private def have(block: Block): Boolean = this have (block.hash)

  private def havePrevOf(block: Block): Boolean = this have block.prevBlockHash

  private def prevOf(block: Block): Option[Block] = this get block.prevBlockHash

  private def prevOfOrError(block: Block): Block = this getOrError block.prevBlockHash

  private def getChainFrom(hash: Hash): Blockchain = {
    return getChainFrom(this getOrError hash)
  }

  private def getChainFrom(block: Block): Blockchain = {
    var blocks = mutable.ListBuffer[Block]()
    blocks.prepend(block)

    var currentBlock = block
    while (this havePrevOf currentBlock) {
      currentBlock = this prevOfOrError currentBlock
      blocks.prepend(currentBlock)
    }
    assert(currentBlock == GenesisBlock, "Got a chain that doesn't start with the GenesisBlock!")

    return new Blockchain(blocks.toList)
  }
}
