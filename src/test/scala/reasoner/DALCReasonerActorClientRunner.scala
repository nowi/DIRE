package reasoner


import allocation.{NaiveOneToOneUnrestrictedLocalAllocator}
import helpers.Logging
import partitioning.ManualClauseStoragePartitioner
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.remote.RemoteNode

/**
 * User: nowi
 * Date: 23.01.2010
 * Time: 13:36:50
 */


object DALCReasonerActorClientRunner extends scala.Application with Logging {
  override def main(args: Array[String]) = {
    // startup some remote nodes a remote node on this machine
    RemoteNode.start("localhost", 9999)
    //RemoteNode.start("localhost", 9998)

    // create some reasoners //

    // one remote
    val remoteActor1 = new DALCReasoningServer with Reasoning with Bootstrapping

    // four local actors
    val localActor1 = new DALCReasoningServer with Reasoning with Bootstrapping
    val localActor2 = new DALCReasoningServer with Reasoning with Bootstrapping
    val localActor4 = new DALCReasoningServer with Reasoning with Bootstrapping
    val localActor3 = new DALCReasoningServer with Reasoning with Bootstrapping


    // start the remote Actor on localhost
    remoteActor1.makeRemote("localhost", 9999)
    // start them up
    remoteActor1.start

    localActor1.start
    localActor2.start
    localActor3.start
    localActor4.start

    val reasoners = List(remoteActor1, localActor1, localActor2, localActor3, localActor4)


    // transfer the ontology
    loadOnotologies(reasoners)


  }


  def loadOnotologies(reasoners: List[Actor with Reasoning with Bootstrapping]) {
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
      log.info("Sending clauses {} to reasoner {}", clauseStore, reasoner)
      reasoner ! LoadClauses(clauseStore)
    }

    // create allocation table , this maps ontology signature --> reasoner

    val allocationTable = allocation.map({case (clauseStore, reasoner) => Map(clauseStore.signature -> reasoner.uuid)}).reduceLeft(_ ++ _)

    // send the allocation table  to the reasoning nodes
    for (reasoner <- reasoners) {
      log.info("Sending allocattionTable {} to reasoner {}", ((allocationTable map {case (sig, uuid) => ("Signature" + "-->" + "Reasoner :" + uuid)}) mkString ("AllocationTable : [\n", ",\n", "]")), reasoner)
      reasoner ! LoadAllocation(allocationTable)
    }


  }


}