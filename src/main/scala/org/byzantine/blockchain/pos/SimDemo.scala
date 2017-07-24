package org.byzantine.blockchain.pos

import java.io.{BufferedWriter, File, FileWriter}
import java.time.Instant

import org.byzantine.blockchain.ProtocolSimulator

object SimDemo extends App {
  val ps = new ProtocolSimulator(5)
  ps.initAll()

  for(r <- 0 until 100) {
    ps.round()
    Thread.sleep(1000)
  }

  val log = ps.log.toString
  println(log)

  val file = new File("log-"+ Instant.now.getEpochSecond + ".txt" )
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(log)
  bw.close()

}
