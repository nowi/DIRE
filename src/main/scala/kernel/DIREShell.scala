
import allocation.NaiveOneToOneUnrestrictedLocalAllocator
import partitioning.ManualClauseStoragePartitioner
import kernel._
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}

//import se.scalablesolutions.akka.actor.Actor.Sender.Self


import se.scalablesolutions.akka.remote.RemoteNode
import se.scalablesolutions.akka.util.Logging

/**
 * User: nowi
 * Date: 08.02.2010
 * Time: 12:14:05
 */

class DIREShell extends Actor with Logging {
  start
  println("Welcome to DIRE shell , enter help for list of available commands")
  println("Enter help(command) for more specific help")
  def help {
    println("List of all  available commands : ")
  }

  def ls = {
    println("Listing all registered reasoners")
    ActorRegistry.actorsFor("kernel.DistributedALCReasoner")
  }

  def ls2 {
    println("List of all  active reasoners : ")
  }


  def status(reasoners: Actor*) {
    for (reasoner <- reasoners) reasoner ! GetStatus(System.currentTimeMillis.toString)
  }


  def startSaturate(reasoners : Actor*){
    broadcast(StartSatisfy("start"),reasoners : _*)
  }


  def keptClauses(reasoners: Actor*) {
    for (reasoner <- reasoners) reasoner ! GetKeptClauses(System.currentTimeMillis.toString)
  }


  def broadcast(message: Event, destinations: Actor*) {
    for (destination <- destinations)
      send(destination, message)

  }

  def send(destination: Actor, message: Event) {
    destination ! message

  }

  def createDefaultReasonerWithBroadCastDispatcher(): Actor = {
    val config = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    new DistributedALCReasoner(config)

  }

  def createDefaultReasonerNoDispatching(): Actor = {
    val config = new Object {
      val dispatcherActor = new ToVoidDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    new DistributedALCReasoner(config)

  }

  def loadOnotologiesAndAllocations(reasoners: List[Actor]) {
    // load the main ontology
    val filenames = List("input/conf/conf1.dire", "input/conf/conf2.dire", "input/conf/conf3.dire", "input/conf/conf4.dire")

    // partition the ontology
    val partitioner = new ManualClauseStoragePartitioner(filenames)
    val partitions = partitioner.partition(core.containers.CNFClauseStore())

    // create allocation of partitions the the reasoning nodes
    val allocator = new NaiveOneToOneUnrestrictedLocalAllocator
    val allocation = allocator.allocate(partitions, reasoners)


    // send the ontologies to the reasoning nodes
    for ((clauseStore, reasoner) <- allocation) {
      log.debug("Sending clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! LoadClauses(clauseStore)
    }

    // create allocation table , this maps ontology signature --> kernel

    val allocationTable = allocation.map({case (clauseStore, reasoner) => Map(clauseStore.signature -> reasoner.uuid)}).reduceLeft(_ ++ _)

    // send the allocation table  to the reasoning nodes
    for (reasoner <- reasoners) {
      log.debug("Sending allocattionTable %s to kernel %s", ((allocationTable map {case (sig, uuid) => ("Signature" + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")), reasoner)
      reasoner ! LoadAllocation(allocationTable)
    }


  }


  protected def receive = {
    case x => {
      log.info("Recieved Response %s \n\n\n ========================================================================", x)
    }

  }


}

object DIREShell {
  val shell = new DIREShell
  shell.start

  def createAndLoadManualPartionedScenario() = {
    val rs = (for(x <- 0 until 5) yield createDefaultReasonerWithBroadCastDispatcher).toList
    rs.foreach(_ start)
    loadOnotologiesAndAllocations(rs)
    rs
  }

  def createAndLoadManualPartitionedScenarioWithoutDispatching() = {
    val rs = (for(x <- 0 until 5) yield createDefaultReasonerNoDispatching).toList
    rs.foreach(_ start)
    loadOnotologiesAndAllocations(rs)
    rs
  }

  def createDefaultReasonerWithBroadCastDispatcher() = shell.createDefaultReasonerWithBroadCastDispatcher
  def createDefaultReasonerNoDispatching() = shell.createDefaultReasonerNoDispatching

  def help = shell.help

  def ls = shell.ls

  def send(destination: Actor, message: Event) = shell.send(destination, message)

  def status(reasoners: Seq[Actor]) = shell.status(reasoners : _*)
  def status(reasoner: Actor) = shell.status(List(reasoner) : _*)

  def keptClauses(reasoners: Seq[Actor]) = shell.keptClauses(reasoners: _*)
  def keptClauses(reasoner: Actor) = shell.keptClauses(List(reasoner): _*)

  def startSaturate(reasoners : Seq[Actor]) = shell.startSaturate(reasoners :_*)
  def startSaturate(reasoner : Actor) = shell.startSaturate(List(reasoner) :_*)


  def broadcast(message: Event, destinations: Seq[Actor]) = shell.broadcast(message,destinations : _*)

  def loadOnotologiesAndAllocations(reasoners: List[Actor]) = shell.loadOnotologiesAndAllocations(reasoners)

}