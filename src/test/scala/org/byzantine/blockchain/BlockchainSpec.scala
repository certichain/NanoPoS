package org.byzantine.blockchain

import org.byzantine.blockchain.pos.{BlockTree, PoSGenesisBlock}
import org.scalatest._

class BlockchainSpec extends FlatSpec with Matchers {
  val mockPOS = Const.GenesisProofOfStake
  trait NewBlockTree {
    val bt = new BlockTree()
  }

  "A new BlockTree" should "have exactly 1 block in it" in new NewBlockTree {
    bt.chain.blocks.length should be (1)
  }

  it should "start with the genesis block" in new NewBlockTree {
    bt.chain.blocks.head should be (PoSGenesisBlock)
  }

  def shouldNotConstruct(f: () => Any): Unit = {
    try {
      val result = f()
      println("If this prints, construction has worked (bad!)")
      assert(false)
    } catch {
      case constructorFail: IllegalArgumentException => assert(true)
    }
  }

  "A blockchain" should "always have length >= 1 (otherwise can't be constructed)" in {
    shouldNotConstruct(() => Blockchain(Nil))
  }

  it should "always start with the genesis block (otherwise can't be constructed)" in {
    val nonGenesisBlocks = List(
      new Block(new Hash(PoSGenesisBlock), List(), mockPOS),
      new Block(new Hash("Genesis"), List(), mockPOS),
      new Block(new Hash("junk"), List(new Coinbase(Address(3))), mockPOS)
    )

    for (block <- nonGenesisBlocks)
      shouldNotConstruct(() => new Blockchain(List(block)))
  }

  "A block" should "have zero or one coinbase transactions (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Block(new Hash(("0")), List(new Coinbase(Address(0)), new Coinbase(Address(0))), mockPOS))
  }

  it should "have coinbases with the correct output amount (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Block(new Hash("0"), List(new Transaction(Const.CoinbaseSourceAddress, Address(1), Const.CoinbaseAmount + 5)), mockPOS))
    shouldNotConstruct(() => new Block(new Hash("0"), List(new Transaction(Const.CoinbaseSourceAddress, Address(1), 2 * Const.CoinbaseAmount)), mockPOS))
  }

  "A transaction" should "have positive amount (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Transaction(Address(1), Address(2), -5))
    shouldNotConstruct(() => new Transaction(Address(1), Address(2), -1233))
  }

}