package argus.macros

import scala.language.experimental.macros
import scala.reflect.api.Universe

/**
  * Common utils for AST munging. Also includes a few helps for constructing type names/paths from json constructs.
  */
class ASTHelpers[U <: Universe](val u: U) {
  import u._

  /**
    * Returns true if the given contains the given annotation
    */
  def hasAnnotation(mods: Modifiers, annotation: String) = mods.annotations.exists {
    _.exists(_.equalsStructure(q"new ${TypeName(annotation)}()"))
  }

  /**
    * Returns the intersection set between two lists of trees (based on the tree's being equal in structure)
    */
  def treesIntersect(l1: List[Tree], l2: List[Tree]) = l1.filter(i1 => l2.exists(i2 => showRaw(i1) == showRaw(i2)))

  def uncapitialize(s: String) = s.head.toLower + s.tail

  /**
    * For a given type (optional including it's path) returns it's companion object
    */
  def companionForType(typ: Tree) = mkSelectPath(typeSelectPathToList(typ))

  /**
    * For a given companion object (optional including it's path) returns it's given type
    */
  def typeForCompanion(typ: Tree) = mkTypeSelectPath(selectPathToList(typ))

  /**
    * Collects and returns case classes that extend the given type.
    * @return A list of tuples. Each tuple contains the path, and class def
    */
  def collectExtendsType(path: List[String], typ: Tree, defs: List[Tree]): List[(List[String], Tree)] = defs.collect {
    case (defDef@q"case object $name extends $sTyp { ..$_ }") if sTyp.equalsStructure(typ) => (path, defDef) :: Nil
    case (defDef@q"case class $name(..$params) extends $sTyp") if sTyp.equalsStructure(typ) => (path, defDef) :: Nil
    case (q"object $name { ..$innerDefs }") => collectExtendsType(path :+ name.toString, typ, innerDefs)
  }.flatten

  /**
    * Returns a string from a given type (with path) that somewhat uniquely identifies this type. Can be useful for
    * producing variable names.
    * For example: a.b.C => ABC
    * @param typ The type to produce a term from.
    * @param usePath Whether to use a type's path or not. If not then only C of a.b.C is used. Defaults to true.
    */
  def nameFromType(typ: Tree, usePath: Boolean = true) = {
    val full = typeSelectPathToList(typ)
    if (usePath) full.map(_.capitalize).mkString else full.last.capitalize
  }

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
  def paramsToMap(nameAndDefaults: List[(String, Any)], params: List[Tree]): Map[String, Any] =
    ParamsASTHelper.paramsToMap(u)(nameAndDefaults, params)
}
