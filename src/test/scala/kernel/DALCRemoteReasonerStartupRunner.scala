package kernel

import se.scalablesolutions.akka.remote.RemoteNode

/**
 * User: nowi
 * Date: 23.01.2010
 * Time: 14:23:07
 */

object DALCRemoteReasonerStartupRunner extends scala.Application {
  override def main(args: Array[String]) = {
    // startup some remote nodes a remote node on this machine
    RemoteNode.start("localhost", 9999)


  }
}