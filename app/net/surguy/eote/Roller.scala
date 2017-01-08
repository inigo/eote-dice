package net.surguy.eote

import controllers.Die
import controllers.DieTypes._
import controllers.Results._

import scala.util.Random

/**
  * Roll a set of dice, and return results based on the type of dice.
  */
class Roller {

  private val dieSides: Map[DieType, List[List[Result]]] = Map(
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
