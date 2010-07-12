package kernel


import helpers.Logging
import se.scalablesolutions.akka.actor.ActorRegistry
import se.scalablesolutions.akka.remote.RemoteNode

/**
 * User: nowi
 * Date: 24.05.2010
 * Time: 16:17:57
 */

object DIREServer extends Application with se.scalablesolutions.akka.actor.Actor with Logging {
  override def main(a: Array[String]) = {
    a.toList match {
      case port :: reasonerType :: _ => {

        se.scalablesolutions.akka.remote.Cluster.start
        RemoteNode.start("localhost", port.toInt)

        // check type

        reasonerType match {
          case "FOL" => {
            println("Staring a Full FOL reasoning node")
            val reasoner = new DefaultFOLReasoner
            log.error("%s my Configuration  is : %s", this, reasoner.toString)

            log.error("%s my ActorRegistry  is : \n", this)
            ActorRegistry.actors.foreach({actor => log.error("%s\n", reasoner)})
            RemoteNode.register("folReasoner", reasoner)
            reasoner.start
          }
          case _ => {
            println("Staring a RDL reasoning node")
            val reasoner = new DefaultDALCReasoner
            log.error("%s my Configuration  is : %s", this, reasoner.toString)

            log.error("%s my ActorRegistry  is : \n", this)
            ActorRegistry.actors.foreach({actor => log.error("%s\n", reasoner)})
            RemoteNode.register("reasoner", reasoner)
            reasoner.start


          }

        }


      }
      case port :: _ => {
        se.scalablesolutions.akka.remote.Cluster.start
        RemoteNode.start("localhost", port.toInt)
        val reasoner = new DefaultDALCReasoner
        log.error("%s my Class  is : %s", this, classOf[DefaultDALCReasoner])
        log.error("%s my Configuration  is : %s", this, reasoner.toString)

        log.error("%s my ActorRegistry  is : \n", this)
        ActorRegistry.actors.foreach({actor => log.error("%s\n", reasoner)})
        RemoteNode.register("reasoner", reasoner)
        reasoner.start
      }
      case _ => {
        error("Arguments not sufficient , specify port and module idetifier for this reasoning peer")
      }
    }

  }


  override protected def receive = {
    case _ => {

      log.info("%s revieved a message")
    }
  }
}

