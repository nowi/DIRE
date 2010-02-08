package reasoner


import allocation.{NaiveOneToOneUnrestrictedLocalAllocator}
import core.config.CuriosityDomain
import domain.fol.ast.StandardClause
import partitioning.ManualClauseStoragePartitioner
import se.scalablesolutions.akka.actor.Actor


import se.scalablesolutions.akka.remote.RemoteNode
import se.scalablesolutions.akka.util.Logging


/**
 * User: nowi
 * Date: 23.01.2010
 * Time: 13:36:50
 */


object DALCReasonerActorClientRunner extends scala.Application with Actor with Logging{
  override def main(args: Array[String]) = {
    // startup some remote nodes a remote node on this machine
    start
    RemoteNode.start("localhost", 9999)
    //RemoteNode.start("localhost", 9998)

    // create the actor env



    // one remote
    val localconfig0 = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    val localActor0 = new DistributedALCReasoner(localconfig0)


    // four local actors
    val config1 = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }

    val localActor1 = new DistributedALCReasoner(config1)


    val config2 = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    val localActor2 = new DistributedALCReasoner(config2)

    val config3 = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    val localActor3 = new DistributedALCReasoner(config3)


    val config4 = new Object {
      val dispatcherActor = new BroadCastDispatchingActor;
      val provingActor = new OrderedResolutionProver1Actor(dispatcherActor)
    }
    val localActor4 = new DistributedALCReasoner(config4)


    //remoteActor1.makeRemote("localhost", 9999)

    localActor0.start
    localActor1.start
    localActor2.start
    localActor3.start
    localActor4.start


    val reasoners = List(localActor0, localActor1, localActor2, localActor3, localActor4)


    // transfer the ontology
    loadOnotologiesAndAllocations(reasoners)

    // start saturation on all
//    reasoners.foreach {_ ! StartSatisfy("Start") }

    // start saturation on remote
    //localActor0 ! StartSatisfy("Start")
   // localActor1 ! StartSatisfy("Start")
    localActor2 ! StartSatisfy("Start")
    localActor3 ! StartSatisfy("Start")
    //localActor4 ! StartSatisfy("Start")


    //println("Results from the reasoners are : " + results)

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
      log.debug("Sending clauses %s to reasoner %s", clauseStore, reasoner)
      reasoner ! LoadClauses(clauseStore)
    }

    // create allocation table , this maps ontology signature --> reasoner

    val allocationTable = allocation.map({case (clauseStore, reasoner) => Map(clauseStore.signature -> reasoner.uuid)}).reduceLeft(_ ++ _)

    // send the allocation table  to the reasoning nodes
    for (reasoner <- reasoners) {
      log.debug("Sending allocattionTable %s to reasoner %s", ((allocationTable map {case (sig, uuid) => ("Signature" + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")), reasoner)
      reasoner ! LoadAllocation(allocationTable)
    }


  }


  protected def receive = {
    case Result(result) => {
         log.info("Recieved Result Message %s",result)
       }
    case (ProverStatus(status),sender :Actor )=> {
         log.info("Recieved STATUS Message from %s ... prover is now %s",sender,status)
       }



  }
}