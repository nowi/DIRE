package core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:03:55
 */

import com.jteigen.scalatest.JUnit4Runner

import config.{Partition1OrderedTheoremProvingConfig}
import containers.{CNFClauseStore}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class Proover1Partition1DomainProovingSpec extends CuriosityDomainProovingSpec {
  override val resolutionProover = new ResolutionProover1(Partition1OrderedTheoremProvingConfig)
}