package metrics


import recording._

/**
 * User: nowi
 * Date: 27.05.2010
 * Time: 17:41:33
 */

trait ReasonerEventAnalyzing {
  val events : Iterable[ReasonerEvent]

  lazy val derived  = events.filter(_.isInstanceOf[DerivedClause])
  lazy val reduced  = events.filter(_.isInstanceOf[ReducedClause])
  lazy val recieved  = events.filter(_.isInstanceOf[RecievedClause])
  lazy val dispatched = events.filter(_.isInstanceOf[DispatchedClause])

  lazy val derivedTotal : Int = {
    derived.toList.size
  }

  lazy val recievedTotal : Int = {
    recieved.toList.size
  }

  lazy val reducedTotal : Int = reduced.toList.size

  lazy val dispatchedTotal : Int = dispatched.toList.size


  lazy val recievedAndReducedClausesPerNode : Map[String,Int] = {
    // extract the reasoners
    var results = Map[String,Int]()
    val nodes = events.map(_.nodeId).toList.removeDuplicates
    for(currentNodeId <- nodes;reducedAtNode = reduced.filter(_.nodeId == currentNodeId);recivedAtNode = recieved.filter(_.nodeId == currentNodeId)){
      results = results + (currentNodeId -> (recivedAtNode.toList -- reducedAtNode.toList).size)
    }
    results
  }

  lazy val recievedAndReducedClausesTotal = recievedAndReducedClausesPerNode.map(_._2).reduceLeft(_ + _)

  lazy val percentageReduced : Double = (reducedTotal).toDouble / (derivedTotal).toDouble

  

}

class ReasonerEventAnalysis(override val events : Iterable[ReasonerEvent]) extends ReasonerEventAnalyzing {




}