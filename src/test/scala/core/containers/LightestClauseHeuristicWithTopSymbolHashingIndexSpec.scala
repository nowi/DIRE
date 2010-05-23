package core.containers

/**
 * User: nowi
 * Date: 20.05.2010
 * Time: 09:48:50
 */
import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import heuristics.LightestClauseHeuristicStorage
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class LightestClauseHeuristicWithTopSymbolHashingIndexSpec extends UsableClausesStoreSpec {
  override def createStorage = new MutableClauseStore with LightestClauseHeuristicStorage with FeatureVectorImperfectIndex
}