package argus.macros

import org.scalactic.Equality

/**
  * @author Aish Fenton.
  */
trait ASTMatchers {

  val runtimeUniverse = scala.reflect.runtime.universe
  import runtimeUniverse._

  // For testing equality between trees in tests
  implicit val treeEq = new Equality[Tree] {
    def areEqual(a: Tree, b: Any): Boolean =
      b match {
        case c: Tree => a.equalsStructure(c)
        case _ => false
      }
  }

  val extractCodecNameAndType: PartialFunction[Tree, (String, String)] = {
    case q"implicit val $name: $typ = $stuff" => (name.toString, typ.toString)
  }

}
