package controllers

import controllers.DieTypes._
import org.specs2.mutable.Specification

class HomeControllerTest extends Specification {

  private val jsonString =
    """
      |[
      |{"name":"ability","value":0},
      |{"name":"proficiency","value":3},
      |{"name":"boost","value":0},
      |{"name":"challenge","value":0},
      |{"name":"difficulty","value":2},
      |{"name":"setback","value":0},
      |{"name":"force","value":1}
      |]
    """.stripMargin

  "parsing JSON" should {
    "generate dietypes" in {
      DieTypeParser.parse(jsonString) mustEqual List(Proficiency, Proficiency, Proficiency, Difficulty, Difficulty, Force)
    }
  }

}
