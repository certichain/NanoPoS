package org.byzantine.blockchain
import org.byzantine.blockchain.pos.{PoSGenesisBlock, ProofOfStake}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

case class SimRef(val id: Int)

trait SimCommunication extends Communication[SimRef] {
  type TaggedTransmission = (Int, SimRef, Transmission) // Int = round, SimRef = source, Transmission = (dst, msg)
}

abstract class SimProcess(pid: Int) extends NodeRole[SimRef] {
  val self = SimRef(pid)

  def init(otherProcesses: Set[SimRef]): ToSend

  type InternalTransition = Function[Any, ToSend]
  val transitions: Seq[InternalTransition]
}

// TODO: make this generic over SimProcess and NetworkEnvironment
class ProtocolSimulator(val numProc: Int) extends SimCommunication {
  private val processMap = new mutable.HashMap[SimRef, SimNode]
  private def processRefs: Set[SimRef] = processMap.keySet.toSet
  private def processes: Set[SimNode] = processMap.values.toSet

  private val messagePool = new mutable.HashSet[TaggedTransmission]
  private val network = new SynchronousNetwork()
  private var currentRound = 0

  val log = new MessageLog()

  def initAll(): Unit = {
    for (pid <- 0 until numProc) {
      processMap += SimRef(pid) -> new SimNode(pid, PoSGenesisBlock)
    }

    // Currently, all processes know about all other processes
    for (proc <- processes) {
      proc.init(processRefs)
    }
  }

  def round(): Unit = {
    // Determine which messages will get delivered
    val deliveredMessages = network.filter(messagePool.toSet, currentRound)
    messagePool.clear()

    // Deliver messages, retrieving responses while you do it
    for (taggedMsg <- deliveredMessages) {
      val transmission = taggedMsg._3
      val msg = transmission._2
      val receiver = processMap(transmission._1)

      if (receiver.step.isDefinedAt(msg)) {
        val responses: ToSend = receiver.step(msg)
        for (resp <- responses) {
          messagePool += Tuple3(currentRound, receiver.self, resp)
        }
      }
    }

    // Run process internal transitions
    for (proc <- processes) {
      for (trans <- proc.transitions) {
        val outbound: ToSend = trans()
        for (out <- outbound) {
          messagePool += Tuple3(currentRound, proc.self, out)
        }
      }
    }

    log ++= messagePool.toSet
    currentRound += 1
  }

}

abstract class NetworkEnvironment extends SimCommunication {
  def filter(messagePool: Set[TaggedTransmission], round: Int): Set[TaggedTransmission]
}

class SynchronousNetwork extends NetworkEnvironment {
  def filter(messagePool: Set[TaggedTransmission], round: Int) = messagePool
}

class MessageLog extends SimCommunication {
  private val log = new mutable.ListBuffer[TaggedTransmission]

  def ++=(messages: Set[TaggedTransmission]): Unit = {
    log ++= messages
  }

  override def toString: String = {
    val sb = new mutable.StringBuilder()
    val rounds: Seq[Int] = log.map(tm => tm._1).toSet.toSeq.sorted

    for (r <- rounds) {
      sb ++= s"Round $r\n"

      val msgs = log.filter(tm => tm._1 == r).map(tm => Tuple3(tm._2, tm._3._2, tm._3._1))
      val sortedMsgs = msgs.sortWith {
        (A, B) => {
          A._1.id < B._1.id ||
            ((A._1.id == B._1.id) && (A._3.id < B._3.id))
        }
      }

      for (i <- 0 until sortedMsgs.size) {
        sb ++= s"  Msg $i: " + sortedMsgs(i) + "\n"
      }
    }

    sb ++= "\n"
    sb.toString()
  }

}
