import org.scalatest._
import org.byzantine.pos._

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

}