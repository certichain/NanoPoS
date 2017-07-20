package org.byzantine.blockchain

import org.byzantine.blockchain.pos.PoSGenesisBlock

import scala.collection.mutable


// TODO This is the state of what?
// Please, clarify this
case class State[P](blocks: List[Block[P]]) {
  private val balanceSheet = new mutable.HashMap[Address, Int]()
  for (block <- blocks) {
    processBlock(block)
  }

  def balance(of: Address): Int = balanceSheet.getOrElse(of, 0)
  private def setBalance(of: Address, amount: Int): Unit = balanceSheet.put(of, amount)

  def processBlock(block: Block[P]): Unit = {
    // To simply the logic: can always transfer CoinbaseAmount from CoinbaseSource
    setBalance(Const.CoinbaseSourceAddress, Const.CoinbaseAmount)

    for (tx <- block.tx) {
      processTransaction(tx)
    }
  }

  private def processTransaction(tx: Transaction): Unit = {
    require(balance(tx.from) >= tx.amount, s"Sender cannot send more than they have.")

    setBalance(tx.from, balance(tx.from) - tx.amount)
    setBalance(tx.to, balance(tx.to) + tx.amount)
  }
}

case class Blockchain[P](blocks: List[Block[P]]) {
  require(blocks.nonEmpty, "Blockchains must have length > 0.")
  require(blocks.head == PoSGenesisBlock, "Blockchains must start at the genesis block.")

  override def toString: String = "Blockchain[" + blocks.mkString(", ") + "]"
  val top: Block[P] = blocks.last

  def compare(that: Blockchain[P]) = this.blocks.length > that.blocks.length
    // scala.math.signum(this.blocks.length - that.blocks.length)

  val state = State(blocks)

}
