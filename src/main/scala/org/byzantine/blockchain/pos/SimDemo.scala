package org.byzantine.blockchain.pos

import java.io.{BufferedWriter, File, FileWriter}
import java.time.Instant

import org.byzantine.blockchain._

object SimDemo extends App {
  val candidateInvariants = Set(new LocalChainLengthIncreases, new KnownBlocksEnlarges)
  val ps = new ProtocolSimulator(100, candidateInvariants)
  ps.initAll()

  for(r <- 0 until 5000) {
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

}
