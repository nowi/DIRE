import allocation.{ConferenceExample1NodeAllocator, ConferenceExample5NodeAllocator, NaiveOneToOneUnrestrictedLocalDistributor, ClauseAllocation}
import collection.mutable.{ListBuffer, HashMap, Map => MMap}
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
import helpers.{DumpableString, HelperFunctions}
import kernel._
import dispatching.{DALCDispatcherActor, ToVoidDispatchingActor}
import net.lag.configgy.Configgy
import org.apache.cassandra.service.{ColumnOrSuperColumn, ColumnPath, ConsistencyLevel}
import partitioning.{ManualConfExampleMerger, ManualConfExamplePartitioner}
import recording.{ReasonerEvent, NaiveClauseRecorder}
import runtime.RichString
import se.scalablesolutions.akka.actor.{ActorRegistry, Actor}
import se.scalablesolutions.akka.config.ScalaConfig.RemoteAddress
import se.scalablesolutions.akka.persistence.cassandra.Protocol.JSON
import se.scalablesolutions.akka.persistence.cassandra.{CassandraStorage, CassandraSessionPool}
import se.scalablesolutions.akka.persistence.common.{StackPool, SocketProvider}
import se.scalablesolutions.akka.persistence.mongo.MongoStorage
import se.scalablesolutions.akka.remote.{RemoteClient, Cluster, RemoteNode}
import se.scalablesolutions.akka.stm.NoTransactionInScopeException
import se.scalablesolutions.akka.util.{Helpers, UUID, Logging}
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import Helpers._
import voldemort.client.{DefaultStoreClient, SocketStoreClientFactory, ClientConfig}
import voldemort.server.{VoldemortServer, VoldemortConfig}
import voldemort.versioning.{Versioned, ArbitraryInconsistencyResolver}
//import se.scalablesolutions.akka.actor.Actor.Sender.Self
import se.scalablesolutions.akka.stm.Transaction._

import com.mongodb.Mongo;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.BasicDBObject;
import com.mongodb.DBObject;
import com.mongodb.DBCursor;


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


  println("Welcome to DIRE shell , enter help for list of available commands")
  println("Enter help(command) for more specific help")
  //val shell = new DIREShell
  println("Starting Jgroups clustering")
  se.scalablesolutions.akka.remote.Cluster.start

  val mongodb: Mongo = new Mongo("localhost");
  val db = mongodb.getDB("mydb");

  val clauseCollection = db.getCollection("clauses")



  val keptClauses: MMap[Actor, Int] = new HashMap()

  val node2Reasoner: MMap[RemoteAddress, Actor] = new HashMap()



  def saveLog(message: String) {
    val doc = new BasicDBObject
    doc.put("message", message);
    clauseCollection.insert(doc);
  }

  def retrieveLogs = {
    val buffer = new ListBuffer[DBObject]()
    val cur : DBCursor = clauseCollection.find()
    while (cur.hasNext()) {
      buffer.append(cur.next())
    }
    buffer.toList.map(_.get("message"))
  }





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
  def localReasoners(count : Int) = {
    for (i <- 0.until(count))yield new DefaultDALCReasoner
  }


  def reasoners(addresses: List[RemoteAddress]): List[Actor] = {
    require(addresses.size <= nodes.size, "We dont have enough computation nodes available in the cluster")
    val rs = new ListBuffer[Actor]
    for (endpoint: RemoteAddress <- addresses) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }


  lazy val _reasoners = reasoners
  lazy val _el = el(_reasoners)

  // get specified number reasoners
  def reasoners(count: Int): List[Actor] = {
    println("Trying to find/spawn % available reasoners in the cluster" format count)
    require(count <= nodes.size, "We dont have enough computation nodes available in the cluster")
    val rs = new ListBuffer[Actor]
    for (endpoint: RemoteAddress <- nodes) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }

  // get rasoners for all available nodes in the cluster
  def reasoners: List[Actor] = {
    println("Trying to find/spawn all available reasoners in the cluster")
    require(!nodes.isEmpty, "There are no availabe computation nodes in cluster %s" format Cluster.toString)
    val rs = new ListBuffer[Actor]
    for (endpoint: RemoteAddress <- nodes) {
      // check if we have a already established remote connection to a reasoner on this adress
      rs += node2Reasoner.getOrElseUpdate(endpoint, RemoteClient.actorFor("reasoner", endpoint.hostname, endpoint.port))
    }
    rs.toList
  }


  def stopReasoners = {
    println("Shutting down all reasoners on compute nodes in cluster %s" format Cluster)
    reasoners.foreach(_.send(Shutdown("bla")))

  }





  ////////////////////////// REASONING ///////////////////////////////




  def status(reasoners: Actor*) = {
    reasoners.map(_ !! GetStatus(System.currentTimeMillis.toString))
  }



  //  def recievedCount(reasoner: Actor) {
  //    reasoner ! GetRecievedClausesCount(System.currentTimeMillis.toString)
  //  }


  def dispatchedCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: ProverStatus = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.dispatchedClauseCount

  }

  def dispatchedCount(reasoners: Iterable[Actor]): Int = {
    reasoners.map(dispatchedCount(_)).reduceLeft(_ + _)
  }

  def keptCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: ProverStatus = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.workedOffCount

  }

  def keptCount(reasoners: Iterable[Actor]): Int = {
    reasoners.map(keptCount(_)).reduceLeft(_ + _)
  }


  def recievedCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: ProverStatus = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.recievedClauseCount

  }

  def recievedCount(reasoners: Iterable[Actor]): Int = {
    reasoners.map(recievedCount(_)).reduceLeft(_ + _)
  }

  def recievedKeptCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: ProverStatus = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.recievedClauseCount

  }

  def recievedKeptCount(reasoners: Iterable[Actor]): Int = {
    reasoners.map(recievedKeptCount(_)).reduceLeft(_ + _)
  }

  def derivedCount(reasoner: Actor): Int = {
    val option = reasoner !! (GetStatus(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: ProverStatus = option.getOrElse(throw new Exception("Couldn't get the count log"))
    recieved.derivedCount

  }

  def derivedCount(reasoners: Iterable[Actor]): Int = {
    reasoners.map(derivedCount(_)).reduceLeft(_ + _)
  }


  def el(reasoner: Actor) = {
    println("Trying to retrieve clause log from reasoner %s" format reasoner)
    val option = reasoner !! (GetEventLog(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val recieved: EventLog = option.getOrElse(throw new Exception("Couldn't get the inference log"))
    recieved.eventLog
  }

  def el(reasoners: Iterable[Actor]) : Iterable[ReasonerEvent] = {
    reasoners.map(el(_)).reduceLeft(_ ++ _)
  }


  def keptClauses(reasoner: Actor): List[FOLClause] = {
    val option = reasoner !! (GetKeptClauses(System.currentTimeMillis.toString), 30000) // timeout 1000 ms
    val kept: KeptClauses = option.getOrElse(throw new Exception("Couldn't get the kept clauses"))
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

  def runOntoFarmCluster: List[Actor] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    require(nodes.size == 5, "We need at least 5 compute nodes in cluster for cluster ontofarm example")

    val ns = nodes.slice(0, 5)
    // spawn reasoners on those nodes
    val rs = reasoners(ns)


    val reasoner2address: Map[Actor, RemoteAddress] = (rs zip ns).foldLeft(Map[Actor, RemoteAddress]())(_ + _)

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


    val allocation = allocator.allocate(partitions, ns)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner ! LoadAllocation(allocation, reasoner2address(reasoner))
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }


  def runOntoFarmLocal : List[Actor] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    // spawn 5 local reasoners
    val rs = localReasoners(5).toList
    val ns = rs.map(_.uuid)


    val reasoner2address: Map[Actor, String] = (rs zip rs.map(_.uuid)).foldLeft(Map[Actor, String]())(_ + _)

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


    val allocation = allocator.allocate(partitions, ns)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner.start
      reasoner ! LoadAllocation(allocation, reasoner2address(reasoner))
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }

  def runOntoFarmMergedLocal : List[Actor] = {
    // spawn remote reasoners

    // echeck if there are enought conpute nodes in the cluster
    // spawn 1 local reasoner
    val rs = localReasoners(1).toList
    val ns = rs.map(_.uuid)


    val reasoner2address: Map[Actor, String] = (rs zip rs.map(_.uuid)).foldLeft(Map[Actor, String]())(_ + _)

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


    val allocation = allocator.allocate(partitions, ns)

    //val allocationTable = (Map() /: allocation)({case (clauseStore, reasoner) => (Map() /: clauseStore.signature)({name : String => Map(name -> reasoner.uuid)})})
    // send the allocation table  to the reasoning nodes
    for (reasoner <- rs) {
      // require(allocation.values.exists(_ == reasoner.uuid), "Missmatch between the uuid in the allocation and the destination actor")
      reasoner.start
      reasoner ! LoadAllocation(allocation, reasoner2address(reasoner))
    }

    // send the ontologies to the reasoning nodes
    for ((reasoner, clauseStore) <- distribution) {
      log.debug("Sending initial clauses %s to kernel %s", clauseStore, reasoner)
      reasoner ! SaturateInitial(clauseStore.toList)
    }

    rs

  }


  //  def loadOnotologiesAndAllocations(rs: List[Actor]) {
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
  //  def loadOnotologiesAndAllocationsMerged(reasoner: Actor) {
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
      keptClauses.put(sender.get, workedOffCount)
      val totalClauses = keptClauses.values.reduceLeft(_ + _)
      log.error("[Total Kept clauses in nodes] : %s \n", totalClauses)


    }

  }




  // CASSSANDRA SHIT




  //  println("Creating Cassandra session pool : %s %s %s" format (CASSANDRA_HOST,CASSANDRA_PORT,CASSANDRA_KEYSPACE))
  //  // create the session pool
  //  val sessions = new CassandraSessionPool(
  //    CASSANDRA_KEYSPACE,
  //    se.scalablesolutions.akka.persistence.common.StackPool(SocketProvider(CASSANDRA_HOST, CASSANDRA_PORT)),
  //    JSON,
  //    ConsistencyLevel.QUORUM)
  //
  //  // cassandra interations
  //
  //  val columnFamily = "Users"
  //  // retrieve a column
  //  val ENCODING = "UTF-8"
  //  val columnName = "screen_name".getBytes(ENCODING)
  //
  //  def find(id: String) = {
  //    val column: Option[ColumnOrSuperColumn] =
  //    sessions.withSession {
  //      session =>
  //              session | (id, new ColumnPath(columnFamily, null, columnName))
  //    }
  //
  //    if (column.isDefined) {
  //      // Convert to ShoppingList
  //      Some(Nil)
  //    } else {
  //      None
  //    }
  //  }
  //
  //  def store(uuid : String,payload : String) = {
  //    // insert a column
  //    val value = payload.getBytes(ENCODING)
  //
  //    sessions.withSession {
  //      session =>
  //        session ++| (uuid,
  //          new ColumnPath(columnFamily, null, columnName),
  //          value,
  //          System.currentTimeMillis)
  //    }
  //  }

}