package org.byzantine.blockchain

import java.time.Instant

/**
  * Block structure, disentangled from the PoS or any other notion of a proof
  *
  * @tparam P proof object, to be instantiated with PoS
  */
class Block[P](val prevBlockHash: Hash, val tx: List[Transaction], val timestamp: Long, val proof: P) {

  require(haveAtMostOneCoinbase, "Blocks must have no more than one coinbase transaction.")
  require(coinbaseHasCorrectAmount, "Blocks cannot contain malformed coinbase transactions.")

  def this(prevBlockHash: Hash, tx: List[Transaction], pos: P) = this(prevBlockHash, tx, Instant.now.getEpochSecond, pos)

  override def toString: String = s"Block($prevBlockHash, $timestamp, [" + tx.mkString(", ") + s"])"

  def hash = new Hash(this)

  private def haveAtMostOneCoinbase = tx.count(t => t.from == Const.CoinbaseSourceAddress) <= 1

  private def coinbaseHasCorrectAmount = !tx.exists(t => t.from == Const.CoinbaseSourceAddress && t.amount != Const.CoinbaseAmount)

}

case class GenesisBlock[P](initP : P)
    extends Block(Const.GenesisPrevHash, List(Const.GenesisCoinbase), Const.GenesisTimestamp, initP)
