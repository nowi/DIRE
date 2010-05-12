package recording.neo4j

import org.neo4j.scala.Neo4jWrapper
import org.specs._
import org.specs.runner._
import org.neo4j.graphdb._
import org.neo4j.kernel.EmbeddedGraphDatabase

class Neo4jWrapperSpecTest extends JUnit4(Neo4jWrapperSpec)

object Neo4jWrapperSpec extends Specification with Neo4jWrapper {
  "NeoWrapper" should {
    shareVariables()
    implicit val neo : GraphDatabaseService = new EmbeddedGraphDatabase("log/graph/clauses")

    Runtime.getRuntime.addShutdownHook(new Thread() {
        override def run() {
          neo.shutdown
        }
      })

    "create a new relationship in --> relType --> notation" in {
      execInNeo4j { neo =>
        val start = neo.createNode

        val end = neo.createNode


        val createdTime = System.currentTimeMillis
        start("created") = createdTime
        start("clause") = domain.fol.ast.StandardClause().toString

        end("created") = createdTime
        end("clause") = domain.fol.ast.StandardClause().toString

        val rel = start.createRelationshipTo(end, DynamicRelationshipType.withName("isParentOf"))
        rel("reasoner") = "reasoner1"

        true

      }
    }

  }
}
