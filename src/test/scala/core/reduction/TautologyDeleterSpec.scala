/**
 * User: nowi
 * Date: 09.12.2009
 * Time: 18:31:26
 */

package core.reduction


import containers.{CNFClauseStore}
import org.junit.runner.RunWith
import com.jteigen.scalatest.JUnit4Runner

/**
 * User: nowi
 * Date: 11.11.2009
 * Time: 15:09:50
 */
@RunWith(classOf[JUnit4Runner])
class TautologyDeleterSpec extends TautologyDeletionSpec {
  override val tautologyDeleter = new TautologyDeleter
}