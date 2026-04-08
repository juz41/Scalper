package controllers

import javax.inject._
import play.api.mvc._

/**
 * Stub HTTP controller – TODO
 * Why: keeps the Play Framework scaffold in place so routing and DI are
 * ready without adding premature complexity.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents)
    extends BaseController {

  def index(): Action[AnyContent] = Action { _ =>
    Ok("Poker API – TODO")
  }
}
