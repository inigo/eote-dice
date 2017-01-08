package net.surguy.eote

import controllers.DieTypes._
import controllers.Results._
import org.specs2.mutable.Specification


class RollerTest extends Specification {

  private val roller = new Roller()
  private val proficiencyDice = List.fill(50)(Proficiency)
  private val forceDice = List.fill(50)(Force)

  "rolling a set of dice" should {
    "return the same set of dice types as were rolled" in {
      val diceToRoll = List(Proficiency, Ability, Force)
      roller.roll(diceToRoll).map(_.dieType) mustEqual diceToRoll
    }
    "return different results when rerolled" in {
      // A tiny amount of the time, this will return the same result - this should be vanishingly unlikely
      roller.roll(proficiencyDice) must not(beEqualTo(roller.roll(proficiencyDice)))
    }
  }

  "rolling proficiency dice" should {
    "never return inappropriate results" in {
      val results = roller.roll(proficiencyDice).flatMap(_.pips)
      results must not(contain(Threat))
      results must not(contain(Failure))
      results must not(contain(Darkside))
      results must not(contain(Lightside))
      results must not(contain(Despair))
    }
    "always return at least one appropriate result" in {
      val results = roller.roll(proficiencyDice).flatMap(_.pips)
      results must contain(Advantage)
      results must contain(Success)
    }
  }

  "rolling force dice" should {
    "always return at least one appropriate result" in {
      val results = roller.roll(forceDice).flatMap(_.pips)
      results must contain(Lightside)
      results must contain(Darkside)
    }
    "never return inappropriate results" in {
      val results = roller.roll(forceDice).flatMap(_.pips)
      results must not(contain(Failure))
      results must not(contain(Threat))
      results must not(contain(Success))
      results must not(contain(Advantage))
    }
  }

}
