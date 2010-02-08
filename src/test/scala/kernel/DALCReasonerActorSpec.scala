package kernel

/**
 * User: nowi
 * Date: 22.01.2010
 * Time: 18:43:35
 */
import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 19:56:30
 */
@RunWith(classOf[JUnit4Runner])
class DALCReasonerActorSpec extends Spec with ShouldMatchers {
  // create a handle to the remote ChatService
  //ChatService.makeRemote("localhost", 9999)
  describe("DALCReasonerActor") {
    it("should connect and send to reasoning actor some simple commands") {
      //      DALCReasoningServer.start
      //      val client = new ReasoningTestClient("jonas")
      //      client.startSatisfy
      ////      client.entail
      //      client.stopSatisfy

    }
  }


}