package controllers

import java.time.Instant
import javax.inject._

import play.api.libs.json._
import play.api.mvc._

/**
 * Return dice values.
 */
@Singleton
class HomeController @Inject() extends Controller {
  import DieTypes._
  import Results._

  implicit val diceWrites = new Writes[Die] {
    def writes(die: Die): JsObject = Json.obj(
      "type" -> die.dieType.name,
      "pips" -> die.pips.map( _.name)
    )
  }

  implicit val rollWrites = new Writes[Roll] {
    def writes(roll: Roll): JsObject = Json.obj(
      "user" -> roll.user,
      "time" -> roll.time.toString,
      "dice" -> roll.dice.map( r => Json.toJson(r) )
    )
  }

  def index = Action {
    Redirect("/assets/index.html")
  }

  def rolls = Action {
    Ok(Json.toJson(
      List(
        Roll("Inigo", Instant.now(), List(Die(Ability, List(Advantage)), Die(Ability, List(Success, Advantage)))),
        Roll("Inigo", Instant.now(), List(Die(Proficiency, List(Advantage, Advantage)), Die(Ability, List(Triumph))))
      )
    ))
  }

}

case class Roll(user: String, time: Instant, dice: List[Die])
case class Die(dieType: DieTypes.DieType, pips: List[Results.Result])

object Results {
  sealed class Result(val name: String)

  case object Lightside extends Result("lightside")
  case object Darkside extends Result("darkside")
  case object Advantage extends Result("advantage")
  case object Threat extends Result("threat")
  case object Success extends Result("success")
  case object Failure extends Result("failure")
  case object Triumph extends Result("triumph")
  case object Despair extends Result("despair")
}

object DieTypes {
  sealed class DieType(val name: String)

  case object Ability extends DieType("ability")
  case object Proficiency extends DieType("proficiency")
  case object Boost extends DieType("boost")
  case object Challenge extends DieType("challenge")
  case object Difficulty extends DieType("difficulty")
  case object Setback extends DieType("setback")
  case object Force extends DieType("force")
}