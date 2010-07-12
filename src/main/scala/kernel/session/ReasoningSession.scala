package kernel.session

import de.unima.dire.allocation._
import de.unima.dire.kernel._
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.util.UUID
import collection.mutable.ListBuffer
import se.scalablesolutions.akka.config.ScalaConfig.RemoteAddress
import se.scalablesolutions.akka.actor.{ActorRegistry, ActorRef, Actor}
import se.scalablesolutions.akka.remote.Cluster

/**
 * User: nowi
 * Date: 08.07.2010
 * Time: 13:04:19
 *
 * A Reasoning Session encapsulates all state and functions that are needed for a individual
 * reasonign task to be performed by DIRE. This includes :
 * +  unique session identifer that is embedded into every event this session creates
 * +  timestamp
 * +  individual strategies for partitioning and allocation
 * +  creation of reasoners
 * +  exectution of reasoning
 * +  resulting resaoning logs
 *
 *
 */

trait ReasoningSession extends Actor {
  // TODO implement this when adding arbitrary ontology support
  //val initialClauses : Seq[FOLClause]
  val allocator: Allocating
  val distributor: Distribution

  val createdOn = System.currentTimeMillis
  val uuid = UUID.newUuid

  val reasoners : Traversable[ActorRef]



  def broadcast(message: Any) {
    println("Broadcasting %s to all reasonsers in session" format message)
    reasoners foreach (_ ! message)
  }

  def nodes: List[RemoteAddress] = {
    println("Retrieving all available nodes in this cluster")
    val endpoints = new ListBuffer[RemoteAddress]
    for (endpoint: RemoteAddress <- Cluster) {
      endpoints += endpoint
    }
    endpoints.toList
  }


  // get number of local reasoners
  def localReasoners(count: Int) = {
    for (i <- 0.until(count)) yield actorOf(new DefaultDALCReasoner)
  }


  def reasoners(addresses: List[RemoteAddress]): List[ActorRef] = {
    require(addresses.size <= nodes.size, "We dont have enough computation nodes available in the cluster")
    val rs = new ListBuffer[ActorRef]
    for (endpoint: RemoteAddress <- addresses) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }


  lazy val _reasoners = reasoners
  lazy val _el = el(_reasoners)

  // get specified number reasoners
  def reasoners(count: Int): List[ActorRef] = {
    println("Trying to find/spawn % available reasoners in the cluster" format count)
    require(count <= nodes.size, "We dont have enough computation nodes available in the cluster")
    val rs = new ListBuffer[ActorRef]
    for (endpoint: RemoteAddress <- nodes) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }


  // get specified number reasoners
  def folReasoners(count: Int): List[ActorRef] = {
    println("Trying to find/spawn % available full fol reasoners in the cluster" format count)
    require(count <= nodes.size, "We dont have enough computation nodes available in the cluster")
    val rs = new ListBuffer[ActorRef]
    for (endpoint: RemoteAddress <- nodes) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("folReasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }

  // get rasoners for all available nodes in the cluster
  def reasoners: List[ActorRef] = {
    println("Trying to find/spawn all available reasoners in the cluster")
    require(!nodes.isEmpty, "There are no availabe computation nodes in cluster %s" format Cluster.toString)
    val rs = new ListBuffer[ActorRef]
    for (endpoint: RemoteAddress <- nodes) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }


  def stopReasoners = {
    println("Shutting down all reasoners on compute nodes in cluster %s" format Cluster)
    reasoners.foreach(_ ! Shutdown("bla"))

  }





  ////////////////////////// REASONING ///////////////////////////////




  def status(reasoners: ActorRef*) = {
    reasoners.map(_ !! GetStatus(System.currentTimeMillis.toString))
  }



  //  def recievedCount(reasoner: ActorRef) {
  //    reasoner ! GetRecievedClausesCount(System.currentTimeMillis.toString)
  //  }


  def dispatchedCount(reasoner: ActorRef): Int = {
    reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) match {
      case Some(recieved: ProverStatus) => recieved.dispatchedClauseCount
      case None => throw new Exception("Couldn't get the count log")
    }
  }

  def dispatchedCount(reasoners: Iterable[ActorRef]): Int = {
    reasoners.map(dispatchedCount(_)).reduceLeft(_ + _)
  }

  def keptCount(reasoner: ActorRef): Int = {
    reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) match {
      case Some(recieved: ProverStatus) => recieved.workedOffCount
      case None => throw new Exception("Couldn't get the count log")
    }

  }

  def keptCount(reasoners: Iterable[ActorRef]): Int = {
    reasoners.map(keptCount(_)).reduceLeft(_ + _)
  }


  def recievedCount(reasoner: ActorRef): Int = {
    reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) match {
      case Some(recieved: ProverStatus) => recieved.recievedClauseCount
      case None => throw new Exception("Couldn't get the count log")
    }
  }

  def recievedCount(reasoners: Iterable[ActorRef]): Int = {
    reasoners.map(recievedCount(_)).reduceLeft(_ + _)
  }

  def recievedKeptCount(reasoner: ActorRef): Int = {
    reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) match {
      case Some(recieved: ProverStatus) => recieved.recievedClauseCount
      case None => throw new Exception("Couldn't get the count log")
    }

  }

  def recievedKeptCount(reasoners: Iterable[ActorRef]): Int = {
    reasoners.map(recievedKeptCount(_)).reduceLeft(_ + _)
  }

  def derivedCount(reasoner: ActorRef): Int = {
    reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) match {
      case Some(status: ProverStatus) => status.derivedCount
      case None => throw new Exception("Couldn't get the count log")
    }

  }

  def derivedCount(reasoners: Iterable[ActorRef]): Int = {
    reasoners.map(derivedCount(_)).reduceLeft(_ + _)
  }


  def el(reasoner: ActorRef) = {
    println("Trying to retrieve clause log from reasoner %s" format reasoner)
    reasoner !! (GetEventLog(System.currentTimeMillis.toString), 30000) match {
      case Some(recieved: EventLog) => recieved.eventLog
      case None => throw new Exception("Couldn't get the inference log ... timeout ?")
    }

  }

  def el(reasoners: Iterable[ActorRef]): Iterable[ReasonerEvent] = {
    reasoners.map(el(_)).reduceLeft(_ ++ _)
  }


  def keptClauses(reasoner: ActorRef): List[FOLClause] = {
    reasoner !! (GetKeptClauses(System.currentTimeMillis.toString), 30000) match {
      case Some(kept: KeptClauses) => kept.clauses.toList
      case None => throw new Exception("Couldn't get the kept clauses")
    }
  }


  def statusOverride(reasoners: ActorRef*) {
    for (reasoner <- reasoners) reasoner ! GetStatusOverride(System.currentTimeMillis.toString)
  }


  def incomingClausesLog(reasoners: ActorRef*) = {
    for (reasoner <- reasoners) reasoner.!(GetIncomingClausesLog(System.currentTimeMillis.toString))
  }


  def startSaturate(reasoners: ActorRef*) {

  }


  def send(destination: ActorRef, message: Event) {
    destination ! message

  }


  def createDALCReasonerWithDALCDispatching(): ActorRef = {
    actorOf(new DefaultDALCReasoner)
  }

  def createDALCReasonerWithBroadCastDispatching(): ActorRef = {
    throw new NotImplementedException
    //    new DALCReasonerBroadCastDispatch

  }


  def runCuriosityExampleCluster: List[ActorRef] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    require(nodes.size == 1, "We need at least 1 compute nodes in cluster for cluster curiosity example")
    // echeck if there are enought conpute nodes in the cluster

    val ns = List(nodes.head)
    // spawn reasoners on those nodes
    val rs = reasoners(ns)



    // partition the ontology
    val partitioner = new ManualConfExamplePartitioner
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, rs)

    // TODO important we need to allocate correctly or else massive overhead

    val allocator = new ConferenceExample5NodeAllocator


    // create allocation of partitions --> RemoteAdress


    val allocation = allocator.allocate(partitions, rs)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner ! LoadAllocation(allocation, reasoner)
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }


  def runOntoFarmCluster: List[ActorRef] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    require(nodes.size == 5, "We need at least 5 compute nodes in cluster for cluster ontofarm example")

    val ns = nodes.slice(0, 5)
    // spawn reasoners on those nodes
    val rs = reasoners(ns)



    // partition the ontology
    val partitioner = new ManualConfExamplePartitioner
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, rs)

    // TODO important we need to allocate correctly or else massive overhead

    val allocator = new ConferenceExample5NodeAllocator


    // create allocation of partitions --> RemoteAdress


    val allocation = allocator.allocate(partitions, rs)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner ! LoadAllocation(allocation, reasoner)
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }


  def runOntoFarmMergedCluster: List[ActorRef] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    require(nodes.size == 1, "We need at least 1 compute nodes in cluster for cluster merged ontofarm example")

    val ns = List(nodes.head)
    // spawn reasoners on those nodes
    val rs = reasoners(ns)



    // partition the ontology
    val partitioner = new ManualConfExampleMerger
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, rs)

    // TODO important we need to allocate correctly or else massive overhead

    val allocator = new ConferenceExample1NodeAllocator


    // create allocation of partitions --> RemoteAdress


    val allocation = allocator.allocate(partitions, rs)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner ! LoadAllocation(allocation, reasoner)
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }


  def runOntoFarmLocal: List[ActorRef] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    // spawn 5 local reasoners
    val rs = localReasoners(5).toList



    // partition the ontology
    val partitioner = new ManualConfExamplePartitioner
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, rs)

    // TODO important we need to allocate correctly or else massive overhead

    val allocator = new ConferenceExample5NodeAllocator


    // create allocation of partitions --> RemoteAdress


    val allocation = allocator.allocate(partitions, rs)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner.start
      reasoner ! LoadAllocation(allocation, reasoner)
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }

  def runOntoFarmMergedLocal: List[ActorRef] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    // spawn 1 local reasoner
    val rs = localReasoners(1).toList


    val reasoner2address: Map[ActorRef, String] = (rs zip rs.map(_.uuid)).foldLeft(Map[ActorRef, String]())(_ + _)

    // partition the ontology
    val partitioner = new ManualConfExampleMerger
    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
    // create allocation of partitions the the reasoning nodes
    // create distribution of partitions the the reasoning nodes
    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
    val distribution = distributor.distribute(partitions, rs)

    // TODO important we need to allocate correctly or else massive overhead

    val allocator = new ConferenceExample1NodeAllocator


    // create allocation of partitions --> RemoteAdress


    val allocation = allocator.allocate(partitions, rs)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner.start
      reasoner ! LoadAllocation(allocation, reasoner)
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }



  protected def receive = {
    case message => {
      println("Session %s recived : %s" format (this, message))
    }

  }

}

// these are hardwired example sessions for the ontofarm example
// this needs to be extended to handle arbitrary input ontologies
// depends on :
// + owl parsing,
// + claussification + definitorial transformations
// + partitioner
case class OntoFarmCentralizedReasoningSession(
        override val allocator: Allocating = new ConferenceExample1NodeAllocator,
        override val distributor: Distribution = new NaiveOneToOneUnrestrictedLocalDistributor) extends ReasoningSession

case class OntoFarmDistributedLocalReasoningSession(
        override val allocator: Allocating = new ConferenceExample5NodeAllocator,
        override val distributor: Distribution = new NaiveOneToOneUnrestrictedLocalDistributor) extends ReasoningSession

case class OntoFarmDistributedRemoteReasoningSession(
        override val allocator: Allocating = new ConferenceExample5NodeAllocator,
        override val distributor: Distribution = new NaiveOneToOneUnrestrictedLocalDistributor) extends ReasoningSession




object ReasoningSession {
  def createOntoFarmCentralizedReasoningSession: ActorRef = actorOf(new OntoFarmCentralizedReasoningSession)
  def createOntoFarmDistributedLocalReasoningSession: ActorRef = actorOf(new OntoFarmDistributedLocalReasoningSession)
  def createOntoFarmDistributedRemoteReasoningSession: ActorRef = actorOf(new OntoFarmDistributedRemoteReasoningSession)

}