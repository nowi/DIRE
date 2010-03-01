package core.resolution

/**
 * User: nowi
 * Date: 13.12.2009
 * Time: 14:38:22
 */
import com.jteigen.scalatest.JUnit4Runner

import config.{CuriosityKilledTheCatOrderedTheoremProvingConfig}
import org.junit.runner.RunWith

import com.jteigen.scalatest.JUnit4Runner

import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import helpers.Logging



@RunWith(classOf[JUnit4Runner])
class OrderedResolverCuriosityDomainSpec extends ALCOrderedResolutionSpec {
  override val resolver = new OrderedResolver(CuriosityKilledTheCatOrderedTheoremProvingConfig)

}