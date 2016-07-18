package argus.macros

import macrocompat.bundle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import argus.schema._
import io.circe.Json

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

  private def extractClassNameFromRef(ref: String) = {
    val parts = ref.stripPrefix("#").stripPrefix("/definitions").split("/")
    parts.map(_.capitalize).mkString
  }

  private def mkIntrinsicType(st: SimpleType): Ident = st match {
    case SimpleTypes.Boolean => tq"Boolean"
    case SimpleTypes.Integer => tq"Int"
    case SimpleTypes.Number => tq"Double"
    case SimpleTypes.String => tq"String"
    case SimpleTypes.Null => tq"Null"
    case _ => throw new Exception("Type isn't a known intrinsic type " + st)
  }

  private def mkCaseClass(name: String, fields: List[Field], requiredFields: Option[List[String]]): List[Tree] = {
    // Build val defs for each field in case class, keeping track of new class defs created along the way (for nested
    // types
    val (params, defs) = fields.foldLeft((List[ValDef](), List[Tree]())) { case ((valDefs, defDefs), field) =>
      // If required, then don't wrap in Option
      val optional = !requiredFields.getOrElse(Nil).contains(field.name)

      val (valDef, defDef) = mkValDef(field, optional)
      (valDefs :+ valDef, defDefs ++ defDef)
    }

    val ccDef = q"""
    case class ${ TypeName(name) } (..$params)
    """

    ccDef :: defs
  }

  private def inOption(inner: Tree) = tq"Option[$inner]"

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

  private def mkEnumDef(baseName: String, enum: List[String]): List[Tree] = {
    val baseTyp = TypeName(baseName)
    val baseDef = q"sealed trait $baseTyp extends scala.Product with scala.Serializable { def json: String }"

    val memberDefs = enum.map { m =>
      val name = TermName(jsonToClassName(m))
      q"case object $name extends $baseTyp { val json = $m }"
    }

    List(baseDef, q"""object ${TermName(baseName + "Enum")} { ..$memberDefs }""")
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
  private def mkTyp(schema: Root, defaultName: String): (Tree, List[Tree]) = {

    // If references existing schema, use that instead
    (schema.typ, schema.$ref, schema.enum) match {

      // A $ref takes precedence over everything
      case (_,Some(ref),_) => {
        val typ = Ident(TypeName(extractClassNameFromRef(ref)))
        (typ, Nil)
      }

      case (_,_,Some(enum)) => {
        val defs = mkDef(defaultName, schema)
        val typ = Ident(TypeName(defaultName))
        (typ, defs)
      }

      // If Object, then type is defined inline, name this type based on defaultName (usually the property key)
      case (Some(SimpleTypeTyp(SimpleTypes.Object)),_,_) => {
        val defs = mkDef(defaultName, schema)
        val typ = Ident(TypeName(defaultName))
        (typ, defs)
      }

      // For Array types, we need to look at "items" attribute
      case (Some(SimpleTypeTyp(SimpleTypes.Array)),_,_) => {
        // TODO
        val typ = Ident(TypeName("Int"))
        (typ, Nil)
      }

      // Make intrinsic type reference
      case (Some(SimpleTypeTyp(st: SimpleType)),_,_) => {
        (mkIntrinsicType(st), Nil)
      }

      case t@_ => (Ident(TermName("Int")), Nil) // throw new Exception("Have no idea what to do with " + t)

    }

  }

  /**
    * Makes a value definition (e.g. val i: Int). These are used for constructing parameter lists
    * and declaring local objects
    */
  private def mkValDef(field: Field, optional: Boolean): (ValDef, List[Tree]) = {
    val schema = field.schema

    val (typ, defs) = mkTyp(schema, field.name.capitalize)

    val valDef = (if (optional) {
      q"val ${TermName(field.name)}: ${inOption(typ)} = None"
    } else {
      q"val ${TermName(field.name)}: $typ"
    }).asInstanceOf[ValDef]

    (valDef, defs)
  }

  private def mkTypeAlias(name: String, typ: String): Tree = mkTypeAlias(name, Ident(typ))
  private def mkTypeAlias(name: String, typ: Ident): Tree = q"type ${TypeName(name)} = $typ"

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
  def mkDef(name: String, schema: Root): List[Tree] = {

    // If references existing schema, use that instead
    (schema.typ, schema.$ref, schema.enum) match {

      // No type specified so either needs a ref, or ??
      case (_,Some(ref),_) => {
        List(mkTypeAlias(name, ref))
      }

      case (_,_,Some(enum)) => {
        mkEnumDef(name, enum)
      }

      // Object, so look into properties to make a new Type
      case (Some(SimpleTypeTyp(SimpleTypes.Object)),_,_) => {
        val defs = mkCaseClass(name, schema.properties.get, schema.required)
        defs
      }

      // For Array types, we need to look at "items" attribute
      case (Some(SimpleTypeTyp(SimpleTypes.Array)),_,_) => {
        // TODO
//        val items = schema.items
        val typ = Ident(TermName("Int"))
        Nil
      }

      // Make intrinsic type
      case (Some(SimpleTypeTyp(st: SimpleType)),_,_) => {
        List(mkTypeAlias(name, mkIntrinsicType(st)))
      }

      // TODO
      case t@_ => Nil // throw new Exception("Have no idea what to do with " + t)

    }

  }

  def mkSchema(name: String, schema: Root): List[Tree] = {

    // Make definitions
    val defdefs = for {
      fields <- schema.definitions.toList
      field <- fields
      defs <- mkDef(field.name.capitalize, field.schema)
    } yield defs

    // Make root
    val rootDefs = mkDef(name, schema)

    defdefs ++ rootDefs
  }

}
