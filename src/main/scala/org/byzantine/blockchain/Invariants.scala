package org.byzantine.blockchain

import scala.collection.mutable

// TODO parameterize over SimProcess
/**
  * Interface:
  *  – the client runs pass at the end of different, given rounds
  *  – the Invariant object collects and stores needed information from the processes every time pass() is called
  *  – when client calls holds(), Invariant looks at stored info and returns whether Invariant is true
  */
trait Invariant {
  def pass(processes: Set[SimNode], round: Long): Unit
  def holds(): Tuple2[Boolean, String]
}


class LocalChainLengthIncreases extends Invariant {
  val chainLength = new mutable.ListBuffer[mutable.HashMap[SimRef, Int]]
  val passes = new mutable.ListBuffer[Long] // passes(k) = round

  def pass(processes: Set[SimNode], round: Long): Unit = {
    val lengths = new mutable.HashMap[SimRef, Int]()
    for (proc <- processes) {
      lengths += proc.self -> proc.chain.blocks.length
    }
    chainLength += lengths
    passes += round
  }

  def holds(): Tuple2[Boolean, String] = {
    require(passes.length >= 2, "Must have at least two passes!")
    var hold = true
    val sb = new mutable.StringBuilder()

    for (i <- 0 until (chainLength.size - 1)) {
      val p0 = chainLength(i)
      val p1 = chainLength(i+1)

      for(p <- p1.keys) {
        hold = hold && (p0(p) <= p1(p))

        if (!hold) {
          sb ++= this.getClass.getName + " does not hold for rounds " + passes(i) + " and " + passes(i+1) + ": \n"
          sb ++= p + "initially has " + p0(p) + " and afterwards has " + p1(p)
        }
      }
    }

    Tuple2(hold, sb.toString)
  }
}

class KnownBlocksEnlarges extends Invariant {
  val knownBlockHashes = new mutable.ListBuffer[mutable.HashMap[SimRef, Set[Hash]]]
  val passes = new mutable.ListBuffer[Long] // passes(k) = round

  def pass(processes: Set[SimNode], round: Long): Unit = {
    val hashes = new mutable.HashMap[SimRef, Set[Hash]]()
    for (proc <- processes) {
      hashes += proc.self -> proc.blockTree.knownBlockHashes
    }
    knownBlockHashes += hashes
    passes += round
  }

  def holds(): Tuple2[Boolean, String] = {
    require(passes.length >= 2, "Must have at least two passes!")
    var hold = true
    val sb = new mutable.StringBuilder()

    for (i <- 0 until (knownBlockHashes.size - 1)) {
      val p0 = knownBlockHashes(i)
      val p1 = knownBlockHashes(i+1)

      for(p <- p1.keys) {
        hold = hold && (p1(p).intersect(p0(p)) == p0(p) && p1(p).size >= p0(p).size)

        if (!hold) {
          sb ++= this.getClass.getName + " does not hold for rounds " + passes(i) + " and " + passes(i+1) + ": \n"
          sb ++= p + "initially has " + p0(p) + " and afterwards has " + p1(p)
        }
      }
    }

    Tuple2(hold, sb.toString)
  }
}
