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

  def makeRoll: Action[JsValue] = Action(parse.json) { request =>
    val user = currentUser(request)
    val time = Instant.now()

    val diceValue = (request.body \ "value").get.toString()
    val dieTypes = DieTypeParser.parse(diceValue)

    val dieResults = roller.roll(dieTypes)
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

  private def currentUser(request: Request[_]) = request.getQueryString("currentUser").getOrElse("Unknown")

}

object DieTypeParser {

  def parse(jsonString: String): List[DieTypes.DieType] = {
    val json: JsValue = Json.parse(jsonString)
    val dieTypes = for (js <- json.asInstanceOf[JsArray].value) yield {
      val name = (js \ "name").get.asInstanceOf[JsString].value
      val count = (js \ "value").get.asInstanceOf[JsNumber].value.toInt
      val dieType = DieTypes.toDieType(name)
      List.fill(count)(dieType)
    }
    dieTypes.flatten.toList
  }

}

class RollsSoFar() {
  private val previousRolls: ListBuffer[Roll] = ListBuffer()

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

  private val DieTypes = List(Ability, Proficiency, Boost, Challenge, Difficulty, Setback, Force)
  def toDieType(name: String): DieType = DieTypes.find(_.name == name).get
}