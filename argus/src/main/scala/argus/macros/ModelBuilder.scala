package argus.macros

import macrocompat.bundle
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import argus.schema._

/**
  * Helper to build the model class for a given schema.
  *
  * @param c The macro reflection context
  * @tparam C wats dis? We need an explicit type to help the type checker see that our path-dependent type
  *           of Context, is actually the same as the one used in the main macro class. ¯\_(ツ)_/¯. #loltypes
  */
class ModelBuilder[C <: Context](val c: C) {
  import Schema._
  import c.universe._

  val helpers = new ASTHelpers(c)
  import helpers._

  val RefPathExtractor = """([\w#]+)/properties""".r("name")
  val RefNameExtractor = """[^/]+$""".r

//  private val IntrinsicType = SimpleTypes.Boolean :: SimpleTypes.String :: SimpleTypes.Number ::
//    SimpleTypes.Integer :: SimpleTypes.Null :: Nil

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

  private def mkIntrinsicType(st: SimpleType): Tree = st match {
    case SimpleTypes.Boolean => tq"Boolean"
    case SimpleTypes.Integer => tq"Int"
    case SimpleTypes.Number => tq"Double"
    case SimpleTypes.String => tq"String"
    case SimpleTypes.Null => tq"Null"
    case _ => throw new Exception("Type isn't a known intrinsic type " + st)
  }

  private def mkCaseClass(path: List[String], name: String, fields: List[Field], requiredFields: Option[List[String]]): List[Tree] = {

    // Build val defs for each field in case class, keeping track of new class defs created along the way (for nested
    // types
    val (params, defs) = fields.foldLeft((List[ValDef](), List[Tree]())) { case ((valDefs, defDefs), field) =>
      // If required, then don't wrap in Option
      val optional = !requiredFields.getOrElse(Nil).contains(field.name)

      val (valDef, defDef) = mkValDef(path :+ name, field, optional)
      (valDefs :+ valDef, defDefs ++ defDef)
    }

    val ccDef = q"""case class ${ TypeName(name) } (..$params)"""
    val companionDef = if (defs.isEmpty) EmptyTree else q"""object ${TermName(name)} { ..$defs }"""

    ccDef :: companionDef :: Nil
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
    * Makes a representation of an enumeration type. We model this as a "sum type" in scala, where each enum is an object
    * that inherits from the base trait. The base trait is sealed so that it can be used for pattern matching.
    *
    * @param baseName Name of the enum (becomes the name of the base trait)
    * @param enum List of all possible enum values
    * @return List[Tree] containing all the definitions
    */
  private def mkEnumDef(path: List[String], baseName: String, enum: List[String]): List[Tree] = {
    val baseTyp = TypeName(baseName)
    val baseDef = q"sealed trait $baseTyp extends scala.Product with scala.Serializable { def json: String }"

    val memberDefs = enum.map { m =>
      val name = TermName(jsonToClassName(m))
      q"case object $name extends $baseTyp { val json = $m }"
    }

    List(baseDef, q"""object ${TermName(baseName + "Enum")} { ..$memberDefs }""")
  }

  /**
    * Json allows type disjunctions, that is: parameters that can take on instances of A OR B. This is modelled in scala using
    * "sum types". Sum types wrap the allowed types (e.g. TypA(a: A), TypB(b: B)), and a common sealed base trait (e.g. Typ)
    * to represent the union.
    *
    * @param baseName The name of the Union wrapper
    * @param schemas A list of allowed sub-types
    * @return A list of definitions created to support the union type.
    */
  private def mkUnionTypeDef(path: List[String], baseName: String, schemas: List[Root]): List[Tree] = {
    val baseTyp = TypeName(baseName)
    val baseDef = q"sealed trait $baseTyp extends scala.Product with scala.Serializable"

    val (memberDefs, defDefs) = schemas.zipWithIndex.foldLeft(List[Tree](), List[Tree]()) { case((md, dd), (schema,i)) =>

      // Hopefully we don't have to use this, but if one of the schema's is an anonymous type then we don't have much
      // choice
      val defaultName = baseName + (i + 1).toString
      val (typ, defs) = mkTyp(path, schema, defaultName)
      val suffix = typ.toString.reverse.takeWhile(_ != '.').reverse

      val name = TypeName(baseName + suffix)
      val memberDef = q"case class $name(x: $typ) extends $baseTyp"
      (md :+ memberDef,  dd ++ defs)

    }

    baseDef :: memberDefs ++ defDefs
  }

  /**
    * Creates a Class/Type definition (i.e. creates a case class or type alias).
    *
    * @param name The name of the class/type to that is created
    * @param schema. The schema that defines the type. Rough set of rules are:
    *   - schema.typ.$ref, creates a type alias
    *   - schema.typ.object, creates a new Case Class
    *   - schema.typ.intrinicType, creates a type alias to the intrinic type
    *   - schmea.typ.array, creates an array based on the type defined within schema.items
    *   - schema.typ.List[st], ???
    * @return A
    */
  def mkDef(path: List[String], name: String, schema: Root): List[Tree] = {

    val multiOf = schema.anyOf.getOrElse(schema.allOf)

    (schema.typ, schema.$ref, schema.enum, schema.oneOf, schema.multiOf) match {

      // No type specified so either needs a ref, or ??
      case (_,Some(ref),_,_,_) => {
        List(mkTypeAlias(path, name, ref))
      }

      case (_,_,Some(enum),_,_) => {
        mkEnumDef(path, name, enum)
      }

      // Object, so look into properties to make a new Type
      case (Some(SimpleTypeTyp(SimpleTypes.Object)),_,_,_,_) => {
        val defs = mkCaseClass(path, name, schema.properties.get, schema.required)
        defs
      }

//      // For Array types, we need to look at "items" attribute
//      case (Some(SimpleTypeTyp(SimpleTypes.Array)),_,_,_,_) => {
//        // TODO
//        //        val items = schema.items
//        val typ = Ident(TermName("Int"))
//        Nil
//      }

      // Make intrinsic type alias
      case (Some(SimpleTypeTyp(st: SimpleType)),_,_,_,_) if IntrinsicType.contains(st) => {
        List(mkTypeAlias(path, name, mkIntrinsicType(st)))
      }

      // Handle one ofs
      case (_,_,_,Some(schemas),_) => {
        mkUnionTypeDef(path, name, schemas)
      }

      // Handle all or any ofs
      case (_,_,_,_,Some(schemas)) => {
//        List(mkTypeAlias(name, mkIntrinsicType(st)))
        Nil
      }

      // TODO
      case t@_ => Nil // throw new Exception("Have no idea what to do with " + t)

    }

  }

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

  /**
    * Extracts a type from the given Schema, using schema.typ or schema.$ref. Usually this means referencing an existing
    * type, be it a $ref or an intrinsic type.
    *
    * However since Schemas can contain nested anonymous types, sometimes as a side-effect we will also create new types
    * (via mkDef()). In this case we name the anon type based on the given defaultName
    *
    * @return A tuple containing the Ident of the type, and a Tree of any addition class definitions
    *         that needed to be generated.
    */
  private def mkTyp(path: List[String], schema: Root, defaultName: String): (Tree, List[Tree]) = {

    // If references existing schema, use that instead
    (schema.typ, schema.$ref) match {

      // A $ref takes precedence over everything
      case (_,Some(ref)) => {
        val typ = mkTypeSelectPath(extractPathFromRef(path, ref))
        println(typ)
        (typ, Nil)
      }

      // Make intrinsic type reference
      case (Some(SimpleTypeTyp(st: SimpleType)),_) if IntrinsicType.contains(st) => {
        (mkIntrinsicType(st), Nil)
      }

      // If not one of the special cases above, then we treat it as an nested definition that needs to be created
      case _ => {
        val defs = mkSchema(path, defaultName, schema)
        val typ = tq"${ mkTypeSelectPath(path :+ defaultName) }"
        (typ, defs)
      }

    }

  }

  /**
    * Makes a value definition (e.g. val i: Int). These are used for constructing parameter lists
    * and declaring local objects
    */
  private def mkValDef(path: List[String], field: Field, optional: Boolean): (ValDef, List[Tree]) = {
    val schema = field.schema

    val (typ, defs) = mkTyp(path, schema, field.name.capitalize)

    val valDef = (if (optional) {
      q"val ${TermName(field.name)}: ${inOption(typ)} = None"
    } else {
      q"val ${TermName(field.name)}: $typ"
    }).asInstanceOf[ValDef]

    (valDef, defs)
  }

  private def mkTypeAlias(path: List[String], name: String, typ: String): Tree = mkTypeAlias(path, name, Ident(TypeName(typ)))
  private def mkTypeAlias(path: List[String], name: String, typ: Tree): Tree = q"type ${TypeName(name)} = $typ"

  def mkSchema(path: List[String], name: String, schema: Root): List[Tree] = {

    // Make definitions
    val defdefs = for {
      fields <- schema.definitions.toList
      field <- fields
      defs <- mkDef(path, field.name.capitalize, field.schema)
    } yield defs

    // Make root
    val rootDefs = mkDef(path, name, schema)

    defdefs ++ rootDefs
  }

}
