package controllers

import play.api.Logger
import play.api.Play.current
import play.api.mvc.Request
import play.api.mvc.Controller
import play.api.mvc.Action
import models._

object AppController extends Controller {
  def index = Action { request =>
    Ok("Hello Arulselvan!")
  }
  def helloTo(name: String) = Action {request => Ok(s"Hello, $name!")}
}
