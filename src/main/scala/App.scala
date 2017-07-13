package org.byzantine.pos

import scala.collection.mutable.ListBuffer
import akka.actor. { Actor, ActorSystem, ActorRef, Props }
import language.postfixOps
import scala.concurrent.duration._
import scala.util.Random

object Akka extends App {
  val system = ActorSystem("pos")
  val N: Int = 10

  // Create nodes
  val nodesBuffer = new ListBuffer[ActorRef]()
  for (i <- 0 to (N-1)) {
    nodesBuffer.append(system.actorOf(Props(classOf[Node], i), i.toString))
  }

  val nodes = nodesBuffer.toList
  // Inform them about each other
  for (initNode <- nodes) {
    for (peerNode <- nodes) {
      if (initNode != peerNode)
        initNode ! new Node.AddPeerMsg(peerNode)
    }
  }

  import system.dispatcher
  def runnable(f: => Unit): Runnable = new Runnable() { def run() = f }

  val rand = new Random()
  def randomNodeID(): Int = rand.nextInt(N)

  // Nodes try to send transactions every once in a while (they might fail if they don't have money)
  system.scheduler.schedule(0 seconds, 5 second, runnable {
    val possibleSender = randomNodeID()
    val receiver = randomNodeID()

    nodes(possibleSender) ! new Node.TransferMsg(new Address(possibleSender), new Address(receiver), 5)
  })

  // All nodes try to mint from time to time
  system.scheduler.schedule(0 seconds, 1 seconds, runnable {
    for (i <- 0 to (N-1)) {
      nodes(i) ! new Node.MintMsg(Address(i))
    }
  })
}
