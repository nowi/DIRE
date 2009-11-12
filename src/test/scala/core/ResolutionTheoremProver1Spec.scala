package core

/**
 * User: nowi
 * Date: 12.11.2009
 * Time: 18:06:30
 */


import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import containers.{CNFClauseStore}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class ResolutionTheoremProver1Spec extends ProovingSpec {
  val resolutionProover = new ResolutionProover1(TheoremProvingConfig1)
}