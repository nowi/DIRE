package core.resolution

/**
 * User: nowi
 * Date: 13.12.2009
 * Time: 14:37:09
 */
import com.jteigen.scalatest.JUnit4Runner

import config.{WestOrderedTheoremProovingConfig}
import containers.{CNFClauseStore}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class BinaryResolverSpec extends ResolutionSpec {
  override val resolver = new BinaryResolver(WestOrderedTheoremProovingConfig)
}