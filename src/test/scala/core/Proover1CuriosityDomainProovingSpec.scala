package core

/**
 * User: nowi
 * Date: 20.01.2010
 * Time: 12:28:41
 */

import com.jteigen.scalatest.JUnit4Runner

import config.CuriosityKilledTheCatOrderedTheoremProvingConfig
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class Proover1CuriosityDomainProovingSpec extends CuriosityDomainProovingSpec {
  override val resolutionProover = new ResolutionProover1(CuriosityKilledTheCatOrderedTheoremProvingConfig)
}