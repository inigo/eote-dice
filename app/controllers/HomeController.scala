package controllers

import java.time.Instant
import javax.inject._

import play.api.libs.json._
import play.api.mvc._

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Return dice values.
 */
@Singleton
class HomeController @Inject() extends Controller {
  import DieTypes._
  import Results._

  val roller = new Roller()

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

  val previousRolls: ListBuffer[Roll] = ListBuffer(
    Roll("Inigo", Instant.now(), List(Die(Ability, List(Advantage)), Die(Ability, List(Success, Advantage)))),
    Roll("Inigo", Instant.now(), List(Die(Proficiency, List(Advantage, Advantage)), Die(Ability, List(Triumph))))
  )

  def index = Action {
    Redirect("/assets/index.html")
  }

  def rolls = Action {
    Ok(Json.toJson(previousRolls.toList.reverse))
  }

  def makeRoll = Action { request =>

    val user = currentUser(request)
    val time = Instant.now()
    val dieResults = roller.roll(List(Ability, Proficiency, Difficulty))
    val roll = Roll(user, time, dieResults)

    previousRolls += roll

    Redirect("/rolls")
  }

  private def currentUser(request: Request[AnyContent]) = "Inigo"
}

class Roller {

  import DieTypes._
  import Results._

  val dieSides: Map[DieType, List[List[Result]]] = Map(
    Proficiency -> List( ds(), ds(Success), ds(Success), ds(Success, Success), ds(Success, Success), ds(Advantage),
      ds(Advantage, Success), ds(Advantage, Success), ds(Advantage, Success), ds(Advantage, Advantage), ds(Advantage, Advantage), ds(Triumph)),
    Ability -> List( ds(), ds(Success), ds(Success), ds(Success, Success), ds(Advantage), ds(Advantage), ds(Success, Advantage), ds(Advantage, Advantage)),
    Boost -> List(ds(), ds(), ds(Advantage, Advantage), ds(Advantage), ds(Success, Advantage), ds(Success)),
    Challenge -> List(ds(), ds(Failure), ds(Failure), ds(Failure, Failure), ds(Failure, Failure), ds(Threat), ds(Threat), ds(Failure, Threat),
      ds(Failure, Threat), ds(Threat, Threat), ds(Threat, Threat), ds(Despair)),
    Difficulty -> List(ds(), ds(Failure), ds(Failure,Failure), ds(Threat), ds(Threat), ds(Threat), ds(Threat, Threat), ds(Failure, Threat)),
    Setback -> List(ds(), ds(), ds(Failure), ds(Failure), ds(Threat), ds(Threat)),
    Force -> List(ds(Darkside), ds(Darkside), ds(Darkside), ds(Darkside), ds(Darkside), ds(Darkside), ds(Darkside, Darkside),
      ds(Lightside), ds(Lightside), ds(Lightside, Lightside), ds(Lightside, Lightside), ds(Lightside, Lightside))
  )

  private def ds(types: Result*): List[Result] = types.toList

  def roll(dice: List[DieType]): List[Die] = {
    dice.map(roll)
  }

  private def roll(die: DieType): Die = {
    val options = dieSides(die)
    val pips = options(Random.nextInt(options.size))
    Die(die, pips)
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