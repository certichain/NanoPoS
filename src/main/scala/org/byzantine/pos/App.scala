package org.byzantine.pos

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.Await
import scala.util.Random

object Akka extends App {
  val system = ActorSystem("pos")
  val N: Int = 10

  // Create nodes
  val nodesBuffer = new ListBuffer[ActorRef]()
  for (i <- 0 until N) {
    nodesBuffer.append(system.actorOf(Props(classOf[Node], i), i.toString))
  }

  val nodes = nodesBuffer.toList
  // Inform them about each other
  for (initNode <- nodes) {
    for (peerNode <- nodes) {
      if (initNode != peerNode)
        initNode ! Node.AddPeerMsg(peerNode)
    }
  }

  import system.dispatcher
  def runnable(f: => Unit): Runnable = new Runnable() { def run() = f }

  // All nodes try to mint from time to time
  system.scheduler.schedule(0 seconds, 1 seconds, runnable {
    for (i <- 0 until N) {
      nodes(i) ! Node.MintMsg(Address(i))
    }
  })

  val rand = new Random()
  def randomNodeID(): Int = rand.nextInt(N)

  // Create random transaction on S (might be invalid)
  var ok = true
  while (ok) {
    val ln = scala.io.StdIn.readLine()

    if (ln.contains('S')) {
      val receiver = randomNodeID()
      val nodeIDs = (0 until N).toList
      var sent = false

      for (sender <- scala.util.Random.shuffle(nodeIDs)) {
        if (!sent) {
          val future = nodes(sender).ask(Node.TransferMsg(Address(sender), Address(receiver), 5))(1 seconds)
          sent = sent || Await.result(future, 1 seconds).asInstanceOf[Boolean]
        }
      }
    }
    else if(ln.contains('M')) {
      nodes.head ! Node.MemPoolMsg
    }
    else if (ln.contains('B')) {
      nodes.head ! Node.BlockchainMsg
    }

    ok = ln != ""
  }

  system.terminate()
}
