package argus.macros
import macrocompat.bundle
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import argus.schema._

/**
  * Common utils to make various AST constructs
  */
class ASTHelpers[C <: Context](val c: C) {
  import Schema._
  import c.universe._

  val RefPathExtractor = """([\w#]+)/properties""".r("name")
  val RefNameExtractor = """[^/]+$""".r

  val IntrinsicType = SimpleTypes.Boolean :: SimpleTypes.String :: SimpleTypes.Number ::
    SimpleTypes.Integer :: SimpleTypes.Null :: Nil

  def mkIntrinsicType(st: SimpleType): Tree = st match {
    case SimpleTypes.Boolean => tq"Boolean"
    case SimpleTypes.Integer => tq"Int"
    case SimpleTypes.Number => tq"Double"
    case SimpleTypes.String => tq"String"
    case SimpleTypes.Null => tq"Null"
    case _ => throw new Exception("Type isn't a known intrinsic type " + st)
  }

  /**
    * Extracts a package path from a json-schema $ref.
    * @param path Current path, although we only use the container + root elements, since $ref should always be an
    *             absolute referneces (relative to container/root/))
    * @param ref The $ref element to decompose.
    */
  private def extractPathFromRef(path: List[String], ref: String): List[String] = {
    require(ref.startsWith("#/"), "references need to be absolute: " + ref)

    // We make every instance of "/x/properties" into a path references. We have to treat the root, #, as a special case
    // and replace it with whatever name we passed configured
    val defPath = RefPathExtractor.findAllMatchIn(ref).map(_.group("name").capitalize).map {
      case "#" => path.tail.head
      case e: String => e
    }.toList

    val name = RefNameExtractor.findFirstIn(ref).get

    path.head :: (defPath :+ name)
  }

  private def inOption(inner: Tree) = { tq"Option[$inner]" }

  /**
    * Best attempt at producing a name from a Json blob. This is mostly used for enum, where the enum's can be any
    * abitary Json structure. Mostly they are plain strings, in which case this is a no-op, but in the case where
    * it's more complex then the property names from json structure are used to build a name. If the json object
    * is a number then decimial points are stripped, and "n" is prefixed (i.e. 3.14 => n314).
    */
  private def jsonToClassName(json: String) =
    json
      .split("[^A-Za-z0-9_.-]+")
      .filterNot(_.isEmpty)
      .map(_.capitalize)
      .map { s => if (s.head.isDigit) "n" + s else s }
      .mkString

  /**
    * Quasiquote don't support building trees of selects (i.e. obj1.obj2.MyObj), so we do it here manually.
    *
    * @param path List of strings representing the paths.
    * @return e.g. obj1.obj2.myObj becomes: Select(Select(Ident(TermName("obj1")), TermName("obj2")), TermName("myObj"))
    */
  private def mkSelectPath(path: List[String]) = {
    def mkPathRec(path: List[String]): Tree = path match {
      case "_root_" :: Nil => Ident(termNames.ROOTPKG)
      case a :: Nil => Ident(TermName(a))
      case a :: xs => Select(mkPathRec(xs), TermName(a))
    }

    mkPathRec(path.reverse)
  }

  /**
    * Builds on mkSelectPath, but treats last item is a Type rather than a TermName
    */
  private def mkTypeSelectPath(path: List[String]): Tree = path match {
    case a :: Nil => Ident(TypeName(a))
    case a :: xs => Select(mkSelectPath(path.dropRight(1)), TypeName(path.last))
  }


}
