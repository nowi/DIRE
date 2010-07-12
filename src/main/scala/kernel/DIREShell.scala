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


object DIREShell extends Application {


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


   def help {
    println("List of all  available commands : ")
  }

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