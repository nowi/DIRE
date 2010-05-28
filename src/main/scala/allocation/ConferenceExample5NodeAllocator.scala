package allocation


import collection.MapProxy
import core.containers.ClauseStorage
import helpers.Logging
import se.scalablesolutions.akka.actor.Actor

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 15:09:44
 */
trait Allocating {
  def allocate(modules: List[ClauseStorage], reasoners: List[Any]): Map[String, Any]
}


// literal sig --> reasoner id
class ClauseAllocation(override val self:Map[String, Any]) extends MapProxy[String, Any]{
    //add other non-Map stuff here
}

class ConferenceExample5NodeAllocator extends Allocating with Logging {
  override def allocate(modules: List[ClauseStorage], addresses: List[Any]): Map[String, Any] = {
    require(modules.size == 5 && addresses.size == 5 , "Illegal number of modules or reasoning nodes for Conf example")
    log.info("%s is allocating modules %s to addresses %s", this, modules, addresses)

    val allClauses = modules.map(_.toList).flatten(i => i)

    val allocation0Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O0" || top.substring(0,3) == "-O0" })
    val allocation1Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O1" || top.substring(0,3) == "-O1" })
    val allocation2Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O2" || top.substring(0,3) == "-O2" })
    val allocation3Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O3" || top.substring(0,3) == "-O3" })
    val allocation4Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O4" || top.substring(0,3) == "-O4" })

    val allocation0 = allocation0Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)
    val allocation1 = allocation1Symbols.map((_,addresses(1))).foldLeft(Map[String, Any]())(_ + _)
    val allocation2 = allocation2Symbols.map((_,addresses(2))).foldLeft(Map[String, Any]())(_ + _)
    val allocation3 = allocation3Symbols.map((_,addresses(3))).foldLeft(Map[String, Any]())(_ + _)
    val allocation4 = allocation4Symbols.map((_,addresses(4))).foldLeft(Map[String, Any]())(_ + _)


    val allocations = allocation0 ++ allocation1 ++ allocation2 ++ allocation3 ++ allocation4

    // blind allocation , a more sophisticated alogorithm could analyze the capabilities of
    // addresses
    allocations
  }

}

class ConferenceExample1NodeAllocator extends Allocating with Logging {
  override def allocate(modules: List[ClauseStorage], addresses: List[Any]): Map[String, Any] = {
    log.info("%s is allocating modules %s to addresses %s", this, modules, addresses)

    val allClauses = modules.map(_.toList).flatten(i => i)

    val allocation0Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O0" || top.substring(0,3) == "-O0" })
    val allocation1Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O1" || top.substring(0,3) == "-O1" })
    val allocation2Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O2" || top.substring(0,3) == "-O2" })
    val allocation3Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O3" || top.substring(0,3) == "-O3" })
    val allocation4Symbols = allClauses.flatMap(_.literals.map(_.top)).filter({top => top.substring(0,2) == "O4" || top.substring(0,3) == "-O4" })

    val allocation0 = allocation0Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)
    val allocation1 = allocation1Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)
    val allocation2 = allocation2Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)
    val allocation3 = allocation3Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)
    val allocation4 = allocation4Symbols.map((_,addresses(0))).foldLeft(Map[String, Any]())(_ + _)


    val allocations = allocation0 ++ allocation1 ++ allocation2 ++ allocation3 ++ allocation4

    // blind allocation , a more sophisticated alogorithm could analyze the capabilities of
    // addresses
    allocations
  }

}