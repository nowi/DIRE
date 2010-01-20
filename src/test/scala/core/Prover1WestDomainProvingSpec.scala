import com.jteigen.scalatest.JUnit4Runner

import core.config.WestOrderedTheoremProovingConfig
import core.{ResolutionProover1, WestDomainProvingSpec}
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class Prover1WestDomainProvingSpec extends WestDomainProvingSpec {
  override val resolutionProover = new ResolutionProover1(WestOrderedTheoremProovingConfig)
}