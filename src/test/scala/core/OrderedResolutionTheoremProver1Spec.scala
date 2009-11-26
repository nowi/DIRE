package core

/**
 * User: nowi
 * Date: 26.11.2009
 * Time: 17:44:46
 */

import com.jteigen.scalatest.JUnit4Runner

import config.OrderedTheoremProvingConfig1
import containers.{CNFClauseStore}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class OrderedResolutionTheoremProver1Spec extends ProovingSpec {
  val resolutionProover = new ResolutionProover1(OrderedTheoremProvingConfig1)
}