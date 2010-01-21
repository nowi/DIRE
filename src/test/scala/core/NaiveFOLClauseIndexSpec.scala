package core

/**
 * User: nowi
 * Date: 20.12.2009
 * Time: 12:33:46
 */
import com.jteigen.scalatest.JUnit4Runner

import containers.{NaiveFOLClauseIndex, FOLClauseIndex, CNFClauseStore}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class NaiveFOLClauseIndexSpec extends FOLClauseIndexSpec {
  val index: FOLClauseIndex = new NaiveFOLClauseIndex()

}