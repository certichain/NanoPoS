package org.byzantine.blockchain

import scala.collection.mutable

// TODO parameterize over SimProcess
/**
  * Interface:
  * – the client runs pass at the end of different, given rounds
  * – the Invariant object collects and stores needed information from the processes every time pass() is called
  * – when client calls holds(), Invariant looks at stored info and returns whether Invariant is true
  */
trait Invariant {

  // TODO: Why can'y you make it in one pass, at the end of the execution,
  // simultaneously collecting the data and checking the invariant?
  // All this lazy initialization with pass/holds may introduce more bugs
  // and makes it hard to debug the implementation and also, when it comes to it,
  // formalize the invariant when moving to Coq.
  def pass(processes: Set[SimNode], round: Long): Unit

  def holds(): (Boolean, String)
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

  def holds(): (Boolean, String) = {
    require(passes.length >= 2, "Must have at least two passes!")
    var hold = true
    val sb = new mutable.StringBuilder()

    for (i <- 0 until (chainLength.size - 1)) {
      val p0 = chainLength(i)
      val p1 = chainLength(i + 1)

      for (p <- p1.keys) {
        val cond = p0(p) <= p1(p)
        hold = hold && cond

        if (!cond) {
          sb ++= s"${this.getClass.getName} does not hold for rounds ${passes(i)} and ${passes(i + 1)}: \n"
          sb ++= s"${p}initially has ${p0(p)} and afterwards has ${p1(p)}\n"
        }
      }
    }

    (hold, sb.toString)
  }
}

class KnownBlocksEnlarges extends Invariant {
  val knownBlockHashes = new mutable.ListBuffer[Map[SimRef, Set[Hash]]]
  val passes = new mutable.ListBuffer[Long] // passes(k) = round

  def pass(processes: Set[SimNode], round: Long): Unit = {
    // Functional programming FTW!
    knownBlockHashes += processes.map(p => p.self -> p.blockTree.knownBlockHashes).toMap
    passes += round
  }

  def holds(): (Boolean, String) = {
    require(passes.length >= 2, "Must have at least two passes!")
    var hold = true
    val sb = new mutable.StringBuilder()

    for (i <- 0 until (knownBlockHashes.size - 1)) {
      val p0 = knownBlockHashes(i)
      val p1 = knownBlockHashes(i + 1)

      for (p <- p1.keys) {
        val cond = p1(p).intersect(p0(p)) == p0(p) && p1(p).size >= p0(p).size
        hold = hold && cond

        if (!cond) {
          sb ++= s"${this.getClass.getName} does not hold for rounds ${passes(i)} and ${passes(i + 1)}: \n"
          sb ++= s"${p}initially has ${p0(p)} and afterwards has ${p1(p)}\n"
        }
      }
    }

    (hold, sb.toString)
  }
}

class AllPeersEventuallyKnown extends Invariant {
  var hold = false
  var knownPeers = Map.empty[SimRef, Set[SimRef]]
  var pr: (Int, Long) = (0, 0)

  def pass(processes: Set[SimNode], round: Long): Unit = {
    knownPeers = processes.map(proc => proc.self -> proc.Peers.knownPeers).toMap

    if (holds()._1) {
      hold = true
    }

    pr = (pr._1 + 1, round)
  }

  def holds(): (Boolean, String) = {
    if (hold) {
      (true, "")
    } else {
      var H = false
      val sb = new mutable.StringBuilder()

      for (p <- knownPeers.keySet.toSeq.sortBy(_.id)) {
        val cond = knownPeers(p).size == knownPeers.size
        H = H || cond
        if (!cond) {
          val knows = knownPeers(p).toSeq.map(_.id).sorted.mkString("[", ",", "]")
          val expected = knownPeers.keySet.toSeq.map(_.id).sorted.mkString("[", ",", "]")
          sb ++= s"$p knows $knows, but should know $expected\n"
        }
      }

      (H, sb.toString)
    }
  }

}
