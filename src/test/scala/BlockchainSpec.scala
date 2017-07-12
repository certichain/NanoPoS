import org.scalatest._
import org.byzantine.pos.{Address, Block, Blockchain, Coinbase, Hash, Transaction, _}

class BlockchainSpec extends FlatSpec with Matchers {
  trait NewBlockTree {
    val bt = new BlockTree()
  }

  "A new BlockTree" should "have exactly 1 block in it" in new NewBlockTree {
    bt.chain.blocks.length should be (1)
  }

  it should "start with the genesis block" in new NewBlockTree {
    bt.chain.blocks(0) should be (GenesisBlock)
  }

  it should "allow extensions building on the genesis block" in new NewBlockTree {
    val newBlock = new Block(new Hash(GenesisBlock), List())
    bt.extend(newBlock)
    bt.top should be (newBlock)
  }

  it should "NOT allow extensions on other hashes" in new NewBlockTree {
    val blocksBuildingOnJunk = List(
      new Block(new Hash("0"), List()),
      new Block(new Hash(new Hash(GenesisBlock)), List()),
      new Block(new Hash("Genesis"), List())
    )

    for (newBlock <- blocksBuildingOnJunk) {
      try {
        bt.extend(newBlock)
      } catch {
        case _: Throwable => "We expect this to fail."
      }

      bt.top should be(GenesisBlock)
    }
  }

  "The ChainForkRule" should "choose the longest chain in case of a fork" in new NewBlockTree {
    // We manually supply the timestamps so the test works regardless of timing
    val f1_block1 = new Block(new Hash(GenesisBlock), List(), 1)
    val f1_block2 = new Block(new Hash(f1_block1), List(), 2)

    val f2_block1 = new Block(new Hash(GenesisBlock), List(), 3)

    bt.extend(f1_block1)
    bt.extend(f1_block2)

    bt.extend(f2_block1)

    bt.top should be (f1_block2)
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
    shouldNotConstruct(() => new Blockchain(List()))
  }

  it should "always start with the genesis block (otherwise can't be constructed)" in {
    val nonGenesisBlocks = List(
      new Block(new Hash(GenesisBlock), List()),
      new Block(new Hash("Genesis"), List()),
      new Block(new Hash("junk"), List(new Coinbase(new Address(3))))
    )

    for (block <- nonGenesisBlocks)
      shouldNotConstruct(() => new Blockchain(List(block)))
  }

  it should "correctly calculate the balance for various addresses" in {
    val bc = new Blockchain(
      List(
        GenesisBlock,
        new Block(GenesisBlock.hash, List(new Coinbase(new Address(1)), new Transaction(new Address(0), new Address(2), 10)))
      )
    )

    bc.State.balance(new Address(1)) should be (Const.CoinbaseAmount)
    bc.State.balance(new Address(2)) should be (10)
    bc.State.balance(new Address(0)) should be (15)
  }

  "A block" should "have zero or one coinbase transactions (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Block(new Hash(("0")), List(new Coinbase(new Address(0)), new Coinbase(new Address(0)))))
  }

  it should "have coinbases with the correct output amount (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Block(new Hash("0"), List(new Transaction(Const.CoinbaseSourceAddress, new Address(1), Const.CoinbaseAmount + 5))))
    shouldNotConstruct(() => new Block(new Hash("0"), List(new Transaction(Const.CoinbaseSourceAddress, new Address(1), 2 * Const.CoinbaseAmount))))
  }

  "A transaction" should "have positive amount (otherwise can't be constructed)" in {
    shouldNotConstruct(() => new Transaction(new Address(1), new Address(2), -5))
    shouldNotConstruct(() => new Transaction(new Address(1), new Address(2), -1233))
  }

}