package argus.macros
import scala.language.experimental.macros
import argus.schema._
import scala.reflect.api.Universe

/**
  * Common utils for AST munging. Also includes a few helps for constructing type names/paths from json constructs.
  */
class ASTHelpers[U <: Universe](val u: U) {
  import u._

  def treesIntersect(l1: List[Tree], l2: List[Tree]) = l1.filter(i1 => l2.exists(i2 => showRaw(i1) == showRaw(i2)))

  def uncapitialize(s: String) = s.head.toLower + s.tail

  def typeNameToTermName(typ: Tree) = mkSelectPath(typeSelectPathToList(typ))
  def termNameToTypeName(typ: Tree) = mkTypeSelectPath(selectPathToList(typ))

  /**
    * Collects and returns case classes that extend the given type.
    * @return A list of tuples. Each tuple contains the path, and class def
    */
  def extendsType(path: List[String], typ: Tree, defs: List[Tree]): List[(List[String], Tree)] = defs collect {
    case (defDef@q"case object $name extends $sTyp { ..$_ }") if sTyp.equalsStructure(typ) => (path, defDef) :: Nil
    case (defDef@q"case class $name(..$params) extends $sTyp") if sTyp.equalsStructure(typ) => (path, defDef) :: Nil
    case (q"object $name { ..$innerDefs }") => extendsType(path :+ name.toString, typ, innerDefs)
  } flatten

  /**
    * Makes type into a name, suitable for use as a variable name. Rules:
    *   - Takes the last item of the select path (i.e. a.b.C -> C)
    *   - Makes initial letter lower-case
    */
  def typeToName(typ: Tree) = {
    val last = typeSelectPathToList(typ).last
    uncapitialize(last)
  }
  def typeToName(typ: TypeName) = uncapitialize(typ.toString)

  val RefPathExtractor = """([\w#]+)/properties""".r("name")
  val RefNameExtractor = """[^/]+$""".r

  /**
    * Best attempt at producing a name from a Json blob. This is mostly used for enum, where the enum's can be any
    * arbitrary Json structure. Mostly they are plain strings, in which case this is a no-op, but in the case where
    * it's more complex then the property names from json structure are used to build a name. If the json object
    * is a number then decimial points are stripped, and "n" is prefixed (i.e. 3.14 => n314).
    */
  def nameFromJson(json: String): String = {
    val res = json
      .split("[^A-Za-z0-9_]+")
      .filterNot(_.isEmpty)
      .map(_.capitalize)
      .mkString

     if (res.head.isDigit) "n" + res else res
  }

  /**
    * Extracts a package path from a json-schema $ref. Notes:
    *   - # is replaced by whatever we call the root schema
    *   - Definitions are defined at the same level as their containing object, i.e. for #/properties/a/definitions/Bob
    *     Bob is defined alongside A, not within it. This can be a bit confusing.
    *
    * @param rootName The name of the root class. Which is used to replace #/ references
    * @param ref The $ref element to path-erize.
    */
  def extractPathFromRef(rootName: Option[String], ref: String): List[String] = {
    require(ref.startsWith("#/"), "references need to be absolute: " + ref)

    // We make every instance of "/x/properties" into a path references.
    val defPath = RefPathExtractor.findAllMatchIn(ref).map(_.group("name").capitalize).map {
      case "#" => rootName.getOrElse(throw new Exception("Trying to resolve $ref, but no path:" + ref))
      case e: String => e
    }.toList

    val name = RefNameExtractor.findFirstIn(ref).get
    defPath :+ name
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
    * Quasiquote doesn't support building trees of selects (i.e. obj1.obj2.MyObj), so we do it here manually.
    *
    * @param path List of strings representing the paths.
    * @return e.g. obj1.obj2.myObj becomes: Select(Select(Ident(TermName("obj1")), TermName("obj2")), TermName("myObj"))
    */
  def mkSelectPath(path: List[String]) = {
    def mkPathRec(path: List[String]): Tree = path match {
      case "_root_" :: Nil => Ident(termNames.ROOTPKG)
      case a :: Nil => Ident(TermName(a))
      case a :: xs => Select(mkPathRec(xs), TermName(a))
      case Nil => throw new Exception("Path can't be empty")
    }

    mkPathRec(path.reverse)
  }

  /**
    * Builds on mkSelectPath, but treats last item is a Type rather than a TermName
    */
  def mkTypeSelectPath(path: List[String]): Tree = path match {
    case a :: Nil => Ident(TypeName(a))
    case a :: xs => Select(mkSelectPath(path.dropRight(1)), TypeName(path.last))
    case Nil => throw new Exception("Path can't be empty")
  }

  /**
    * Extract the select path as a List[String]
    */
  def selectPathToList(tree: Tree): List[String] = {
    def selectPathToListRec(tree: Tree): List[String] = tree match {
      case Select(s: Tree, tn: TermName) => tn.toString :: selectPathToListRec(s)
      case Ident(tn: TermName) => tn.toString :: Nil
    }
    selectPathToListRec(tree).reverse
  }

  /**
    * Extract the select path (ending with a TypeName) as a List[String]. Types with type parameters
    * are concatenated to form a single string. For example:
    *   a.b.MyObj[A,B[C]] becomes List(a,b,MyObjABC)
    */
  def typeSelectPathToList(typ: Tree): List[String] = typ match {
    case Ident(TypeName(tn)) => tn.toString :: Nil
    case Select(s: Tree, tn: TypeName) => selectPathToList(s) :+ tn.toString
    case AppliedTypeTree(t: Tree, l: List[Tree]) => {
      val path = typeSelectPathToList(t)
      path.dropRight(1) :+ (path.last + l.flatMap(typeSelectPathToList).mkString)
    }
  }

  /**
    * Extracts argument values from a List[Tree] as is obtained from a quasiquote such as q"myFunc(..$params))".
    * Supports named arguments, and default values.
    * @param nameAndDefaults A list of tuples containing each argument's name and default value. This must be the complete
    *                        list of supported arguments.
    * @param params A list of trees to as extracted by ..$params. If arguments are constants or option wrapped constants
    *               then their reified values are returned (i.e. Some(1)), if not then the AST chunk is returned.
    *
    * @return A map of argument names and their extracted values (or their default value if not extracted)
    */
  def paramsToMap(nameAndDefaults: List[(String, Any)], params: List[Tree]): Map[String, Any] = {

    def toValue(param: Tree): Any = param match {
      case Ident(TermName("None")) => None
      case Apply(Ident(TermName("Some")), List(Literal(Constant(value)))) => Some(value)
      case Literal(Constant(c)) => c
      case _ => param
    }

    // Handle positional vs. named argument separately
    val (positional, named) = params splitAt (params prefixLength  {
      case AssignOrNamedArg(Ident(TermName(name)), _) => false; case _ => true })

    require(positional.length <= nameAndDefaults.length, "More position arguments than specified in: " + nameAndDefaults)
    val posValues = nameAndDefaults zip positional map { case ((name, default), param) =>
      (name, toValue(param))
    } toMap

    val namedValues = named map { case AssignOrNamedArg(Ident(TermName(name)), t: Tree) =>
      (name, toValue(t))
    } toMap

    nameAndDefaults.toMap ++ posValues ++ namedValues
  }

}
