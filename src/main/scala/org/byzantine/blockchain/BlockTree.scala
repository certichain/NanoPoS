package org.byzantine.blockchain

import scala.collection.mutable

/**
  *
  * @param initBlock genesis block
  * @tparam P a type of "proof"
  */
class BlockTree[P](initBlock : GenesisBlock[P]) {

  type MyBlock = Block[P]
  type MyBlockchain = Blockchain[P]

  // TODO: Refactor it so blocks with topHash would be the only mutable structure
  private val blocks = new mutable.HashMap[Hash, MyBlock]()
  private var topHash = add(initBlock)

  private val missingParents = new mutable.HashSet[Hash]()
  private val orphanBlocks = new mutable.HashSet[MyBlock]()

  def top: MyBlock = {
    get(topHash) match {
      case Some(x) => x
      case None => throw new Exception("Our blockchain has no top!")
    }
  }

  def chain: MyBlockchain = buildChainFrom(topHash)

  def extensionPossibleWith(block: MyBlock): Boolean = {
    val constructedChain: Option[MyBlockchain] = try {
      Some(this buildBlockchainFrom block)
    } catch {
      case _: Throwable => None
    }

    constructedChain.isDefined
  }

  def extend(block: MyBlock): Unit = {
    // Keep track of blocks that build on blocks we don't yet have
    if (!hasPrevOf(block)) {
      missingParents += block.prevBlockHash
      orphanBlocks += block
    }
    else {
      if (!has(block) && extensionPossibleWith(block)) {
        val currentChain = chain
        val candidateChain = this buildBlockchainFrom block

        topHash = if (candidateChain.compare(currentChain)) add(block) else topHash

        // This block is the parent of at least one orphan block, so let's extend with the orphan(s) as well
        if (missingParents.contains(block.hash)) {
          val children = orphanBlocks.filter(b => b.prevBlockHash == block.hash)
          for (c <- children) {
            extend(c)
          }
          missingParents -= block.hash
        }
      }
    }
  }

  private def add(block: MyBlock): Hash = {
    blocks.put(block.hash, block)
    block.hash
  }

  def knownBlockHashes: Set[Hash] = blocks.keys.toSet

  def get(hash: Hash): Option[MyBlock] = blocks.get(hash)

  def getOrError(hash: Hash): MyBlock = {
    get(hash) match {
      case Some(x) => x
      case None => throw new Exception(s"We don't have block with hash $hash!")
    }
  }

  def has(hash: Hash): Boolean = {
    get(hash) match {
      case Some(x) => true
      case None => false
    }
  }

  def has(block: MyBlock): Boolean = has(block.hash)

  private def hasPrevOf(block: MyBlock): Boolean = has(block.prevBlockHash)

  private def prevOf(block: MyBlock): Option[MyBlock] = get(block.prevBlockHash)

  private def prevOfOrError(block: MyBlock): MyBlock = getOrError(block.prevBlockHash)

  private def buildChainFrom(hash: Hash): MyBlockchain = buildBlockchainFrom(getOrError(hash))

  private def buildBlockchainFrom(block: MyBlock): MyBlockchain = {
    val blocks = mutable.ListBuffer[MyBlock](block)

    var currentBlock = block
    while (hasPrevOf(currentBlock)) {
      currentBlock = prevOfOrError(currentBlock)
      blocks.append(currentBlock)
    }

    val chain = blocks.toList.reverse
    assert(chain.head == initBlock, "Got a chain that doesn't start with the GenesisBlock!")

    Blockchain(chain)
  }
}
