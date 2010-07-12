package recording


import domain.fol.ast.{ALCDClause, StandardClause, Predicate, FOLClause}
import neo4j.models.Predicates._
import neo4j.Neo4jWrapper
import org.neo4j.index.IndexService
import org.neo4j.index.lucene.LuceneIndexService
import org.neo4j.graphdb._
import org.neo4j.kernel.EmbeddedGraphDatabase
import se.scalablesolutions.akka.util.Logging

/**
 * User: nowi
 * Date: 17.03.2010
 * Time: 17:12:36
 */

class Neo4JRecorder(val path: String) extends ClauseRecording with Neo4jWrapper with Logging {
  implicit val neo: GraphDatabaseService = new EmbeddedGraphDatabase(path)

  Runtime.getRuntime.addShutdownHook(new Thread() {
    override def run() {
      neo.shutdown
    }
  })




  override def toList = Nil

  val index = new LuceneIndexService(neo);


  val parentsOf = (start: Node) =>
          start.traverse(Traverser.Order.BREADTH_FIRST, (tp: TraversalPosition) => false, ReturnableEvaluator.ALL_BUT_START_NODE, ISPARENTOF, Direction.INCOMING).iterator


  private def retrieveNodeForClause(clause: FOLClause) = {
    execInNeo4j {
      neo =>
              index.getSingleNode("clause", clause.toString) match {
                case null => None
                case node : Node => Some(node)
              }
    }
  }

  private def containsClause(clause: FOLClause): Boolean = {

    execInNeo4j {
      neo =>
              val iterable = index.getNodes("clause", clause.toString)
              (iterable.size > 0)
    }

  }


  def ifNotAlreadyInGraph(clause: FOLClause)(block: => Unit) {
    containsClause(clause) match {
      case false => {
        execInNeo4j {
          neo =>
                  block
        }
      }
      case true => {
        log.error("The clause : %s is already persisted in this graph", clause)
        error("The clause : %s is already persisted in this graph" format clause)
      }
    }
  }


  // TODO remove overloading here

  override def record(clause: FOLClause, recieved: Boolean) {
    ifNotAlreadyInGraph(clause) {
      val node = neo.createNode
      val createdTime = System.currentTimeMillis
      val createdBy = this.toString
      node("name") = clause.toString
      node("createdOn") = createdTime
      node("createdBy") = createdBy
      node("clause") = clause.toString
      if (recieved) node("recieved") = "true"
      index.index(node, "clause", node.getProperty("clause"));
    }

  }

  override protected def record(clause: FOLClause, parent1: FOLClause, parent2: FOLClause, recieved: Boolean) {
    execInNeo4j {
      neo =>
              // retrieve the parent nodes
              var p1Node: Node = null
              var p2Node: Node = null

              val iterable1 = index.getNodes("clause", parent1.toString)
              val iter1 = iterable1.iterator
              // TODO workaround Neo4J lucene indexer bug .. hmm makes me wonder
              // wo usses this stuff really ... hmmm
              // http://www.mail-archive.com/user@lists.neo4j.org/msg02442.html
              if (iterable1.size > 1) {
                log.error("There are %s number of parents for this clause  !!!", iterable1.size)
                while (iter1.hasNext()) {
                  val node = iter1.next()
                  log.error("one parent is  : %s !!!", node.getProperty("clause"))
                  p1Node = node
                }
              } else {
                log.info("Fetchign neo node for parent clause : %s",parent1.toString)
                p1Node = iter1.next()

              }

              val iterable2 = index.getNodes("clause", parent2.toString)
              val iter2 = iterable2.iterator
              if (iterable2.size > 1) {
                log.error("There are %s number of parents for this clause  !!!", iterable2.size)
                while (iter2.hasNext()) {
                  val node = iter2.next()
                  log.error("one parent is  : %s !!!", node.getProperty("clause"))
                  p2Node = node
                }
              } else {
                log.info("Fetchign neo node for parent clause : %s",parent2.toString)
                p2Node = iter2.next()

              }


              require(p1Node != null && p2Node != null)


              retrieveNodeForClause(clause) match {
                case None => {
                  // create the derived node
                  val clauseNode = neo.createNode
                  val createdTime = System.currentTimeMillis
                  val createdBy = this.toString

                  clauseNode("createdOn") = createdTime
                  clauseNode("createdBy") = createdBy
                  clauseNode("clause") = clause.toString
                  clauseNode("name") = clause.toString
                  if (recieved) clauseNode("recieved") = "true"

                  // index new node
                  index.index(clauseNode, "clause", clauseNode.getProperty("clause"));

                  // link
                  p1Node.createRelationshipTo(clauseNode, ISPARENTOF)
                  p2Node.createRelationshipTo(clauseNode, ISPARENTOF)

                }

                case Some(node) => {
                  // already in the graph only add new links
                  // link
                  log.info("The clause : %s is already persisted in this graph and has been derived again", clause)
                  p1Node.createRelationshipTo(node, ISPARENTOF)
                  p2Node.createRelationshipTo(node, ISPARENTOF)

                }

              }

    }


  }


  override def getParentsOf(clause: FOLClause): Option[Tuple2[String, String]] = {
    // retrieve the node for this clause

    var clauseNode: Node = null

    execInNeo4j {
      neo =>
              val iterable = index.getNodes("clause", clause.toString)
              val iter = iterable.iterator
              if (iterable.size == 1) {
                clauseNode = iter.next()
              } else if (iterable.size > 1) {
                log.error("There are multiple nodes representing the clause %s", clause)
                clauseNode = iter.next()
              } else {
                log.error("There are no nodes representing the clause %s", clause)
              }
    }


    require(clauseNode != null)

    // get the parents with traverser
    val iter = parentsOf(clauseNode)
    var parent1Node: Node = null
    var parent2Node: Node = null

    execInNeo4j {
      neo =>
              if (iter.hasNext) {
                parent1Node = iter.next
                parent2Node = iter.next
                require(parent1Node != null && parent2Node != null)
                val parent1ClauseString: String = parent1Node.getProperty("clause").asInstanceOf[String]
                val parent2ClauseString: String = parent2Node.getProperty("clause").asInstanceOf[String]
                Some(Tuple2(parent1ClauseString, parent2ClauseString))
              } else {
                None

              }

    }


  }
}