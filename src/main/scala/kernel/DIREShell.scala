import allocation.{ConferenceExample5NodeAllocator, NaiveOneToOneUnrestrictedLocalDistributor, ClauseAllocation}
import collection.mutable.{HashMap, Map => MMap}
import core.caches.{SelectedLitCache, URLitCache}
import core.containers.heuristics.{ListBufferStorage, LightestClauseHeuristicStorage}
import core.containers.{SForrestIndex, MutableClauseStore, CNFClauseStore}
import core.ordering.{CustomConferencePartitionedPrecedence, ALCLPOComparator}
import core.reduction.{ForwardSubsumer, BackwardSubsumer, StillmannSubsumer}
import core.resolution._
import core.rewriting.VariableRewriter
import core.selection.DALCRSelector
import core.{Standardizer, RobinsonProver}
import domain.fol.ast.FOLClause
import kernel._
import dispatching.{DALCDispatcherActor, ToVoidDispatchingActor}
import net.lag.configgy.Configgy
import partitioning.{ManualConfExampleMerger, ManualConfExamplePartitioner}
import recording.NaiveClauseRecorder
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

//import se.scalablesolutions.akka.actor.Actor.Sender.Self


import se.scalablesolutions.akka.remote.RemoteNode
import se.scalablesolutions.akka.util.Logging

/**
 * User: nowi
 * Date: 08.02.2010
 * Time: 12:14:05
 */

class DIREShell extends Actor with Logging {
  Configgy.configure("config/config.conf")

  val keptClauses: MMap[Actor, Int] = new HashMap()


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




  //  def recievedCount(reasoner: Actor) {
  //    reasoner ! GetRecievedClausesCount(System.currentTimeMillis.toString)
  //  }

  def recievedCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetRecievedClausesCount(System.currentTimeMillis.toString), 5000) // timeout 1000 ms
    val recieved: RecievedClausesCount = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.count

  }

  def dispatchedCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetDispatchedClausesCount(System.currentTimeMillis.toString), 5000) // timeout 1000 ms
    val dispatchedClausesCount: DispatchedClausesCount = option.getOrElse(throw new Exception("Couldn't get the count log"))
    dispatchedClausesCount.count

  }

  def keptClauses(reasoner: Actor): List[FOLClause] = {
    val option = reasoner !! (GetKeptClauses(System.currentTimeMillis.toString), 5000) // timeout 1000 ms
    val kept: KeptClauses = option.getOrElse(throw new Exception("Couldn't get the count log"))
    kept.clauses.toList
  }


  def statusOverride(reasoners: Actor*) {
    for (reasoner <- reasoners) reasoner ! GetStatusOverride(System.currentTimeMillis.toString)
  }


  def incomingClausesLog(reasoners: Actor*) = {
    for (reasoner <- reasoners) reasoner.!(GetIncomingClausesLog(System.currentTimeMillis.toString))
  }


  def startSaturate(reasoners: Actor*) {

  }


  def broadcast(message: Event, destinations: Actor*) {
    for (destination <- destinations)
      send(destination, message)

  }

  def send(destination: Actor, message: Event) {
    destination ! message

  }



  def createDALCReasonerWithDALCDispatching(): Actor = {
    new DefaultDALCReasoner
  }

  def createDALCReasonerWithBroadCastDispatching(): Actor = {
    throw new NotImplementedException
    //    new DALCReasonerBroadCastDispatch

  }


  def loadOnotologiesAndAllocations(reasoners: List[Actor]) {
    // partition the ontology
    val partitioner = new ManualConfExamplePartitioner
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, reasoners)

    // TODO important we need to allocate correctly or else massive overhead


    // create allocation table , this maps ontology signature --> kernel
    val allocator = new ConferenceExample5NodeAllocator
    val allocation = allocator.allocate(partitions,reasoners)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for(reasoner <- reasoners)
      reasoner ! LoadAllocation(allocation)

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! Saturate(clauseStore.toList)
    }
  }

  def loadOnotologiesAndAllocationsMerged(reasoner: Actor) {
    val reasoners = List(reasoner)

    // partition the ontology
    val partitioner = new ManualConfExampleMerger
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store

    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, reasoners)

    // create allocation table , this maps ontology signature --> kernel
    val allocator = new ConferenceExample5NodeAllocator
    val allocation = allocator.allocate(partitions,reasoners)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for(reasoner <- reasoners)
      reasoner ! LoadAllocation(allocation)

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! Saturate(clauseStore.toList)
    }
  }


  protected def receive = {
    case ProverStatus(state, workedOffCount, derivedCount,_,_,_) => {
      keptClauses.put(sender.get, workedOffCount)
      val totalClauses = keptClauses.values.reduceLeft(_ + _)
      log.error("[Total Kept clauses in nodes] : %s \n", totalClauses)


    }

  }


}

object DIREShellRunner extends Application {
  override def main(a: Array[String]) = {
    val shell = DIREShell
//    shell.createAndLoadAutoMergedScenario
    //    val mergedKept = shell.keptClauses.values.toList.flatten(itr => itr)
    //    shell.keptClauses.clear
    shell.createAndLoadManualPartionedScenario
    //    val partitionKept = shell.keptClauses.values.toList.flatten(itr => itr)
    //
    //    val difference = mergedKept -- partitionKept
    //
    //    log.warning("[Difference ] : %s \n", difference)

  }
}

object DIREShell {
  val shell = new DIREShell
  shell.start

  val keptClauses = shell.keptClauses

//  def testSingleNodeReasoning {
//    val config = new Object {
//      //Configgy.configure("config/config.conf")
//      // the initial clause store
//      lazy val variableRewriter = new VariableRewriter
//      lazy val standardizer = new Standardizer(this)


  def createAndLoadManualPartionedScenario() = {
    val rs = (for (x <- 0 until 5) yield createDALCReasonerWithDALCDispatching).toList
    rs.foreach(_ start)
    loadOnotologiesAndAllocations(rs)
    rs
  }

  def createAndLoadAutoMergedScenario() = {
    val rs = createDALCReasonerWithDALCDispatching
    rs.start
    loadOnotologiesAndAllocationsMerged(rs)
    rs
  }




  def createDALCReasonerWithDALCDispatching = shell.createDALCReasonerWithDALCDispatching


  def help = shell.help

  def ls = shell.ls

  def send(destination: Actor, message: Event) = shell.send(destination, message)

  def status(reasoners: Seq[Actor]) = shell.status(reasoners: _*)

  def status(reasoner: Actor) = shell.status(List(reasoner): _*)

  def dispatchedCount(reasoner: Actor) = shell.dispatchedCount(reasoner)

  def recievedCount(reasoner: Actor) = shell.recievedCount(reasoner)

  def statusOverride(reasoners: Seq[Actor]) = shell.statusOverride(reasoners: _*)

  def statusOverride(reasoner: Actor) = shell.statusOverride(List(reasoner): _*)

  def incomingClausesLog(reasoners: Seq[Actor]) = shell.incomingClausesLog(reasoners: _*)

  def incomingClausesLog(reasoner: Actor) = shell.incomingClausesLog(List(reasoner): _*)

  def keptClauses(reasoner: Actor) = shell.keptClauses(reasoner)

  def startSaturate(reasoners: Seq[Actor]) = shell.startSaturate(reasoners: _*)

  def startSaturate(reasoner: Actor) = shell.startSaturate(List(reasoner): _*)


  def broadcast(message: Event, destinations: Seq[Actor]) = shell.broadcast(message, destinations: _*)

  def loadOnotologiesAndAllocations(reasoners: List[Actor]) = shell.loadOnotologiesAndAllocations(reasoners)

  def loadOnotologiesAndAllocationsMerged(reasoner: Actor) = shell.loadOnotologiesAndAllocationsMerged(reasoner)

}