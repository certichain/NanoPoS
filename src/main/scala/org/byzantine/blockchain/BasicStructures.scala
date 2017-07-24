package org.byzantine.blockchain

import java.time.Instant

import com.roundeights.hasher.Implicits._
import org.byzantine.blockchain.pos.ProofOfStake

/**
  * Basic structures for BlockChain implementation
  */

// TODO: what are these constants?
object Const {
  val CoinbaseSourceAddress: Address = Address(-1)
  val CoinbaseAmount: Int = 25

  val GenesisPrevHash: Hash = new Hash("Genesis")
  val GenesisCoinbase: Coinbase = new Coinbase(Address(0))
  val GenesisTimestamp: Long = Instant.now.getEpochSecond
  val GenesisProofOfStake = ProofOfStake(GenesisTimestamp, GenesisPrevHash, CoinbaseSourceAddress)

  val MaxAcceptedTimestampDiff: Long = 3600
  val HashTarget: String = "07" + GenesisPrevHash.toString.substring(2)
}

// TODO: comment on this
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


// TODO: comment on this
case class Address(addr: Int) {
  override def toString: String = addr.toString

  override def equals(o: Any) = o match {
    case that: Address => that.addr == this.addr
    case _ => false
  }

  override def hashCode = addr.hashCode
}

// We are assuming that these can't be forged (e.g. they're cryptographically signed by the sender)
class Transaction(val from: Address, val to: Address, val amount: Int, val timestamp: Long) {
  require(amount >= 0, "Cannot send negative amounts.")

  def this(from: Address, to: Address, amount: Int) = this(from, to, amount, Instant.now.getEpochSecond)

  def hash = new Hash(this)

  override def toString: String = s"Tx($from, $to, $amount, $timestamp)"
}

class Coinbase(to: Address) extends Transaction(Const.CoinbaseSourceAddress, to, Const.CoinbaseAmount) {
  override def toString: String = s"Coinbase($to, $amount)"
}



