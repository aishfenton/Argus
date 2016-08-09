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
        // equalsStructure bug: https://github.com/scalamacros/paradise/issues/80
        case c: Tree => showRaw(a) == showRaw(c) //.equalsStructure(c)
        case _ => false
      }
  }

  implicit val valDefEq = new Equality[ValDef] {
    def areEqual(a: ValDef, b: Any): Boolean =
      b match {
        case c: ValDef => showRaw(a) == showRaw(c)
        case _ => false
      }
  }

  implicit val listTreeEq = new Equality[List[Tree]] {
    def areEqual(a: List[Tree], b: Any): Boolean =
      b match {
        case c: List[_] => a.size == c.size && a.zip(c).forall { case(x,y) => treeEq.areEqual(x,y) }
        case _ => false
      }
  }

  val extractCodecNameAndType: PartialFunction[Tree, (String, String)] = {
    case q"implicit val $name: $typ = $_" => (name.toString, typ.toString)
  }

}
