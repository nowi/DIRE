package de.unima.dire


import de.unima.dire.allocation.{ConferenceExample5NodeAllocator, ConferenceExample1NodeAllocator, NaiveOneToOneUnrestrictedLocalDistributor}
import de.unima.dire.partitioning.{ManualConfExamplePartitioner, ManualConfExampleMerger}
import de.unima.dire.recording.ReasonerEvent
import de.unima.dire.core.containers.{CNFClauseStore, FOLClause}
import de.unima.dire.kernel._
import collection.mutable.{ListBuffer, HashMap, Map => MMap}
import se.scalablesolutions.akka.config.ScalaConfig.RemoteAddress
import se.scalablesolutions.akka.remote.{RemoteClient, Cluster}
import net.lag.configgy.Configgy
import se.scalablesolutions.akka.actor.{ActorRef, ActorRegistry, Actor}
import se.scalablesolutions.akka.actor.Actor._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: nowi
 * Date: 08.02.2010
 * Time: 12:14:05
 */


object DIREShell extends Application with Actor {


  //Configgy.configure("/workspace/DIRE/DIRE/config/config.conf")
  val config = Configgy.config

  //  val CASSANDRA_HOST = voldeConfig.getString("cassandra.hostname", "127.0.0.1")
  //  val CASSANDRA_KEYSPACE = voldeConfig.getString("cassandra.keyspace", "DIRE")
  //  val CASSANDRA_PORT = voldeConfig.getInt("cassandra.port", 9160)
  //  val FRAME_LENGTH = voldeConfig.getInt("akka.remote.client.frame-length", 104857600)


  //  println("Welcome to DIRE shell , enter help for list of available commands")
  //  println("Enter help(command) for more specific help")
  //val shell = new DIREShell
  println("Starting Jgroups clustering")
  se.scalablesolutions.akka.remote.Cluster.start




  val keptClauses: MMap[ActorRef, Int] = new HashMap()

  // TODO FIXME this needs to be refactored for N : M deployments
  val node2Reasoner: MMap[RemoteAddress, ActorRef] = new HashMap()


  //  def saveLog(message: String) {
  //    val doc = new BasicDBObject
  //    doc.put("message", message);
  //    clauseCollection.insert(doc);
  //  }
  //
  //  def retrieveLogs = {
  //    val buffer = new ListBuffer[DBObject]()
  //    val cur: DBCursor = clauseCollection.find()
  //    while (cur.hasNext()) {
  //      buffer.append(cur.next())
  //    }
  //    buffer.toList.map(_.get("message"))
  //  }
  //




  // ADMINISTRATION

  def help {
    println("List of all  available commands : ")
  }

  def ls = {
    println("Listing all cluster nodes")
    for (endpoint <- Cluster) println(endpoint)
  }

  def ds = {
    println("Listing all known actors")
    ActorRegistry.actors.foreach(println(_))
  }


  def broadcast(message: Any) {
    println("Broadcasting %s to all reasonsers in cluster" format message)
    for (endpoint: RemoteAddress <- Cluster) {
      se.scalablesolutions.akka.remote.RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port) ! message
    }

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


  //  def loadOnotologiesAndAllocations(rs: List[ActorRef]) {
  //    // partition the ontology
  //    val partitioner = new ManualConfExamplePartitioner
  //    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
  //    // create allocation of partitions the the reasoning nodes
  //    // create distribution of partitions the the reasoning nodes
  //    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
  //    val distribution = distributor.distribute(partitions, rs)
  //
  //    // TODO important we need to allocate correctly or else massive overhead
  //
  //
  //
  //
  //    // create allocation table , this maps ontology signature --> kernel
  //    val allocator = new ConferenceExample5NodeAllocator
  //    val allocation = allocator.allocate(partitions, rs)
  //
  //    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
  //    // send the allocation table  to the reasoning nodes
  //    for (reasoner <- rs) {
  //      require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
  //      reasoner ! LoadAllocation(allocation)
  //    }
  //
  //    // send the ontologies to the reasoning nodes
  //    for ((reasoner, clauseStore) <- distribution) {
  //      log.debug("Sending clauses %s to kernel %s", clauseStore, reasoner)
  //      reasoner ! Saturate(clauseStore.toList)
  //    }
  //  }
  //
  //  def loadOnotologiesAndAllocationsMerged(reasoner: ActorRef) {
  //    val reasoners = List(reasoner)
  //
  //    // partition the ontology
  //    val partitioner = new ManualConfExampleMerger
  //    val partitions = partitioner.partition(CNFClauseStore()) // pass dummy empty store
  //
  //    // create distribution of partitions the the reasoning nodes
  //    val distributor = new NaiveOneToOneUnrestrictedLocalDistributor
  //    val distribution = distributor.distribute(partitions, reasoners)
  //
  //    // create allocation table , this maps ontology signature --> kernel
  //    val allocator = new ConferenceExample5NodeAllocator
  //    val allocation = allocator.allocate(partitions, reasoners)
  //
  //
  //
  //
  //
  //    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
  //    // send the allocation table  to the reasoning nodes
  //    for (reasoner <- reasoners)
  //      reasoner ! LoadAllocation(allocation)
  //
  //    // send the ontologies to the reasoning nodes
  //    for ((reasoner, clauseStore) <- distribution) {
  //      log.debug("Sending clauses %s to kernel %s", clauseStore, reasoner)
  //      reasoner ! Saturate(clauseStore.toList)
  //    }
  //  }


  protected def receive = {
    case ProverStatus(state, workedOffCount, derivedCount, _, _, _) => {

      self.sender match {
        case Some(sender) => {
          keptClauses.put(sender, workedOffCount)
          val totalClauses = keptClauses.values.reduceLeft(_ + _)
          log.error("[Total Kept clauses in nodes] : %s \n", totalClauses)
        }
        case None => //

      }


    }

  }


}


object DIRENewShell {
  def main(args: Array[String]) {
    //Let's assume all arguments are file locations for configuration

    val interpreter = new InterpreterWrapper {
      def prompt = "DIRE> "

      def welcomeMsg = "Welcome to the DIRE interactive mode"

      def helpMsg = ":help will print this message, :quit will exit the DIRE shell"

      autoImport("DIREShell._")
      autoImport("core._")
      autoImport("helpers._")
      autoImport("kernel._")
      autoImport("recording._")
      autoImport("domain._")

      //bind("shell", new DIREShell())
      //OR if I want to restrict access to an interface
      //bindAs("processingBuilder2", classOf[ProcessingBuilder], new MyProcessingBuilderImplementationClass())
      for (fileName <- args) {
        addScriptFile(fileName)
      }
    }
    interpreter.startInterpreting()
  }
}