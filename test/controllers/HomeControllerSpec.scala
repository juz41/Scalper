package controllers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.FakeRequest
import play.api.test.Helpers._

class HomeControllerSpec extends AnyWordSpec with Matchers {

  "HomeController.index" should {
    "return the running status response" in {
      val controller = new HomeController(stubControllerComponents())

      val result = controller.index()(FakeRequest())

      status(result) shouldBe OK
      contentAsString(result) shouldBe "Poker Server Running"
    }
  }
}