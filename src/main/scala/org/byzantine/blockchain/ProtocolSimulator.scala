package org.byzantine.blockchain

import org.byzantine.blockchain.pos.PoSGenesisBlock

import scala.collection.mutable
import scala.util.Random

case class SimRef(id: Int)

trait SimCommunication extends Communication[SimRef] {
  type TaggedTransmission = (Long, SimRef, Transmission) // Long = round, SimRef = source, Transmission = (dst, msg)
}

abstract class SimProcess(pid: Int) extends NodeRole[SimRef] {
  val self = SimRef(pid)
  var round: Long // Automatically updated by Simulator

  def init(otherProcesses: Set[SimRef]): ToSend

  type InternalTransition = Function[Any, ToSend]
  val transitions: Seq[InternalTransition]
}

// TODO: make this generic over SimProcess and NetworkEnvironment
class ProtocolSimulator[Inv <: Invariant](val numProc: Int, numKnown: Int, val invariants: Set[Inv]) extends SimCommunication {
  private val processMap = new mutable.HashMap[SimRef, SimNode]
  private def processRefs: Set[SimRef] = processMap.keySet.toSet
  private def processes: Set[SimNode] = processMap.values.toSet

  private val messagePool = new mutable.HashSet[TaggedTransmission]
  private val network = new SynchronousNetwork()
  private var currentRound: Long = 0

  val log = new MessageLog()

  def initAll(): Unit = {
    for (pid <- 0 until numProc) {
      processMap += SimRef(pid) -> new SimNode(pid, PoSGenesisBlock)
    }

    for (proc <- processes) {
      val knownProcs = Random.shuffle(processRefs.toList).take(numKnown).toSet
      proc.init(knownProcs)
    }

    // First pass for all invariants
    for (inv <- invariants) {
      inv.pass(processes, -1)
    }
  }

  def round(): Unit = {
    // Determine which messages will get delivered
    val deliveredMessages = network.filter(messagePool.toSet, currentRound)
    messagePool --= deliveredMessages

    // Update internal round counter for each process
    for (proc <- processes) {
      proc.round = currentRound
    }

    // Deliver messages, retrieving responses while you do it
    for {
      taggedMsg <- deliveredMessages
      transmission = taggedMsg._3
      msg = transmission._2
      receiver = processMap(transmission._1)
      if receiver.step.isDefinedAt(msg)
      resp <- receiver.step(msg)
    } messagePool += ((currentRound, receiver.self, resp))

    // Run process internal transitions
    for {
      proc <- processes
      trans <- proc.transitions
      outbound: ToSend = trans()
      out <- outbound
    } messagePool += ((currentRound, proc.self, out))

    // Pass through invariants
    for (inv <- invariants) {
      inv.pass(processes, currentRound)
    }

    log ++= deliveredMessages
    currentRound += 1
  }

  def checkInvariants(): (Boolean, String) = {
    val sb = new mutable.StringBuilder()
    var hold = true

    for (inv <- invariants) {
      val check = inv.holds()
      hold = hold && check._1
      sb ++= inv.getClass.getName + (if (check._1) " holds" else " DOESN'T hold") + "\n"
      if (!check._1) {
        sb ++= check._2
      }
    }

    (hold, sb.toString)
  }

}

abstract class NetworkEnvironment extends SimCommunication {
  def filter(messagePool: Set[TaggedTransmission], round: Long): Set[TaggedTransmission]
}

class SynchronousNetwork extends NetworkEnvironment {
  def filter(messagePool: Set[TaggedTransmission], round: Long) = messagePool
}

class MessageLog extends SimCommunication {
  private val log = new mutable.ListBuffer[TaggedTransmission]

  def ++=(messages: Set[TaggedTransmission]): Unit = {
    log ++= messages
  }

  override def toString: String = {
    val sb = new mutable.StringBuilder()
    val rounds: Seq[Long] = log.map(tm => tm._1).toSet.toSeq.sorted

    for (r <- rounds) {
      sb ++= s"Round $r\n"

      val msgs = log.filter(tm => tm._1 == r).map(tm => (tm._2, tm._3._2, tm._3._1))
      val sortedMsgs = msgs.sortWith {
        (A, B) => {
          A._1.id < B._1.id ||
              ((A._1.id == B._1.id) && (A._3.id < B._3.id))
        }
      }

      for (i <- sortedMsgs.indices) {
        sb ++= s"  Msg $i: " + sortedMsgs(i) + "\n"
      }
    }

    sb ++= "\n"
    sb.toString()
  }
}
