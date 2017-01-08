package controllers

import java.time.Instant
import javax.inject._

import net.surguy.eote.Roller
import play.api.libs.json._
import play.api.mvc._

import scala.collection.mutable.ListBuffer

/**
 * Return dice values.
 */
@Singleton
class HomeController @Inject() extends Controller {
  import DieTypes._

  val roller = new Roller()
  val rollsSoFar = new RollsSoFar()

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
    Ok(Json.toJson(rollsSoFar.getPreviousRolls))
  }

  def makeRoll = Action { request =>
    val user = currentUser(request)
    val time = Instant.now()
    val dieResults = roller.roll(List(Ability, Proficiency, Difficulty))
    val roll = Roll(user, time, dieResults)

    rollsSoFar.addRoll(roll)

    Redirect("/rolls")
  }

  /**
    * Retrieve all the rolls if there are any new ones.
    *
    * @param ifChangedSince - expected to be the last modified date previously passed to the client
    * @return HTTP 200 and all the rolls if modified, or HTTP 304 if not
    */
  def rollsIfModified(ifChangedSince: String) = Action {
    val since = Instant.parse(ifChangedSince)
    val lastModified = rollsSoFar.lastModified
    if (lastModified.isAfter(since)) {
      Ok(Json.toJson(rollsSoFar.getPreviousRolls))
    } else {
      NotModified
    }
  }

  private def currentUser(request: Request[AnyContent]) = request.getQueryString("currentUser").getOrElse("Unknown")

}

class RollsSoFar() {
  import controllers.DieTypes._
  import controllers.Results._

  private val previousRolls: ListBuffer[Roll] = ListBuffer(
    Roll("Inigo", Instant.now(), List(Die(Ability, List(Advantage)), Die(Ability, List(Success, Advantage)))),
    Roll("Inigo", Instant.now(), List(Die(Proficiency, List(Advantage, Advantage)), Die(Ability, List(Triumph))))
  )

  def getPreviousRolls: List[Roll] = previousRolls.reverse.toList
  def addRoll(roll: Roll): Unit = previousRolls += roll
  def lastModified: Instant = previousRolls.last.time
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