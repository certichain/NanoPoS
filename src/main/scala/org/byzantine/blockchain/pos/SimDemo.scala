package org.byzantine.blockchain.pos

import java.io.{BufferedWriter, File, FileWriter}
import java.time.Instant

import org.byzantine.blockchain._

object SimDemo extends App {
  val N = 10
  val knownPeers: Int = math.sqrt(N).toInt
  val R = 1000
  val candidateInvariants = Set(new LocalChainLengthIncreases, new KnownBlocksEnlarges, new AllPeersEventuallyKnown)

  val ps = new ProtocolSimulator(N, knownPeers, candidateInvariants)
  ps.initAll()

  for(r <- 0 until R) {
    ps.round()
    println(r)
  }

  val timestamp = Instant.now.getEpochSecond

  println("Checking invariants...")
  val inv = ps.checkInvariants()._2

  println("Writing log...")
  val log = PoSGenesisBlock.toString + "\n" + inv + "\n" + ps.log.toString

  println("Writing to disk...")
  val file = new File("log-"+ timestamp + ".txt" )
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(log)
  bw.close()

  println(inv)
}
