package argus.schema

import org.scalatest._
import scala.language.experimental.macros
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

/**
  * @author Aish Fenton.
  */
class ASTHelpersSpec extends FlatSpec with Matchers {

  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._

  "InOption" should "wrap a given type in Option[X]" in {

    val i = tq"Int"

  }

}
