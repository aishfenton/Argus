package argus.macros
import scala.language.experimental.macros
import argus.schema._
import scala.reflect.api.Universe

/**
  * Common utils for AST munging. Also includes a few helps for constructing type names/paths from json constructs.
  */
class ASTHelpers[U <: Universe](val u: U) {
  import u._

  /**
    * Makes type into a TermName, named the same as type but with initial letter lower-cased
    */
  def typeToTermName(typ: TypeName) = ((s: String) => TermName(s.head.toLower + s.tail))(typ.toString)

  val RefPathExtractor = """([\w#]+)/properties""".r("name")
  val RefNameExtractor = """[^/]+$""".r

  /**
    * Best attempt at producing a name from a Json blob. This is mostly used for enum, where the enum's can be any
    * arbitrary Json structure. Mostly they are plain strings, in which case this is a no-op, but in the case where
    * it's more complex then the property names from json structure are used to build a name. If the json object
    * is a number then decimial points are stripped, and "n" is prefixed (i.e. 3.14 => n314).
    */
  def typeNameFromJson(json: String) = {
    val res = json
      .split("[^A-Za-z0-9_]+")
      .filterNot(_.isEmpty)
      .map(_.capitalize)
      .mkString

     if (res.head.isDigit) "n" + res else res
  }

  /**
    * Extracts a package path from a json-schema $ref. Notes:
    *   - Every path is relative to the container object, a kind of pre-root, which we assume is in path.head
    *   - # is replaced by whatever we call the root schema
    *   - Definitions are defined at the same level as their containing object, i.e. for #/properties/a/definitions/Bob
    *     Bob is defined alongside A, not within it. This can be a bit confusing.
    *
    * @param basePath The name of the container class and the root class. This is all that is required since $ref should
    *                 always be an absolute reference.
    * @param ref The $ref element to path-erize.
    */
  def extractPathFromRef(basePath: List[String], ref: String): List[String] = {
    require(ref.startsWith("#/"), "references need to be absolute: " + ref)

    // We make every instance of "/x/properties" into a path references.
    val defPath = RefPathExtractor.findAllMatchIn(ref).map(_.group("name").capitalize).map {
      case "#" => basePath.tail.head
      case e: String => e
    }.toList

    val name = RefNameExtractor.findFirstIn(ref).get

    basePath.head :: (defPath :+ name)
  }

  /**
    * Wrap the given type in an Option type
    */
  def inOption(inner: Tree) = { tq"Option[$inner]" }

  /**
    * Wrap the given type in an List type
    */
  def inList(inner: Tree) = { tq"List[$inner]" }

  /**
    * Quasiquote don't support building trees of selects (i.e. obj1.obj2.MyObj), so we do it here manually.
    *
    * @param path List of strings representing the paths.
    * @return e.g. obj1.obj2.myObj becomes: Select(Select(Ident(TermName("obj1")), TermName("obj2")), TermName("myObj"))
    */
  def mkSelectPath(path: List[String]) = {
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
  def mkTypeSelectPath(path: List[String]): Tree = path match {
    case a :: Nil => Ident(TypeName(a))
    case a :: xs => Select(mkSelectPath(path.dropRight(1)), TypeName(path.last))
  }


}
