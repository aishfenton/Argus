package argus.macros

import macrocompat.bundle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import argus.schema._

import scala.reflect.api.Universe

/**
  * Helper to build the model class for a given schema.
  *
  * @param u The reflection universe. Can support macro and runtime reflection
  * @tparam U wats dis? We need an explicit type to help the type checker see that our path-dependent type
  *           of Universe, is actually the same as the one used in the main macro class. ¯\_(ツ)_/¯. #loltypes
  */
class ModelBuilder[U <: Universe](val u: U) {
  import Schema._
  import u._

  val helpers = new ASTHelpers[u.type](u)
  import helpers._

  val IntrinsicType = SimpleTypes.Boolean :: SimpleTypes.String :: SimpleTypes.Number ::
    SimpleTypes.Integer :: SimpleTypes.Null :: Nil

  /**
    * Make a type from a SimpleType, when st is a built in type (i.e. Boolean, Int, Double, String, or Null)
    */
  def mkIntrinsicType(st: SimpleType): Tree = st match {
    case SimpleTypes.Boolean => tq"Boolean"
    case SimpleTypes.Integer => tq"Int"
    case SimpleTypes.Number => tq"Double"
    case SimpleTypes.String => tq"String"
    case SimpleTypes.Null => tq"Null"
    case _ => throw new Exception("Type isn't a known intrinsic type " + st)
  }

  def mkCaseClassDef(path: List[String], name: String, fields: List[Field], requiredFields: Option[List[String]]): List[Tree] = {

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

  /**
    * Makes a representation of an enumeration type. We model this as a "sum type" in scala, where each enum is an object
    * that inherits from the base trait. The base trait is sealed so that it can be used for pattern matching.
    *
    * @param baseName Name of the enum (becomes the name of the base trait)
    * @param enum List of all possible enum values
    * @return List[Tree] containing all the definitions
    */
  def mkEnumDef(path: List[String], baseName: String, enum: List[String]): List[Tree] = {
    val baseTyp = TypeName(baseName)
    val baseDef = q"sealed trait $baseTyp extends scala.Product with scala.Serializable { def json: String }"

    val memberDefs = enum.map { m =>
      val name = TermName(typeNameFromJson(m))
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
  def mkUnionTypeDef(path: List[String], baseName: String, schemas: List[Root]): List[Tree] = {
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

  def mkArrayDef(path: List[String], schema: Root) = EmptyTree

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

    (schema.$ref, schema.enum, schema.typ,  schema.oneOf, schema.multiOf) match {

      // No type specified so either needs a ref, or ??
      case (Some(ref),_,_,_,_) => {
        List(mkTypeAlias(path, name, ref))
      }

      case (_,Some(enum),_,_,_) => {
        mkEnumDef(path, name, enum)
      }

      // Object, so look into properties to make a new Type
      case (_,_,Some(SimpleTypeTyp(SimpleTypes.Object)),_,_) => {
        val defs = mkCaseClassDef(path, name, schema.properties.get, schema.required)
        defs
      }

      // Array, create type alias to List wrapper of internal schema
      case (_,_,Some(SimpleTypeTyp(SimpleTypes.Array)),_,_) => {
        val (typ, defs) = mkTyp(path, schema, name + "Item")
        mkTypeAlias(path, name, typ) :: defs
      }

      case (_,_,Some(ListSimpleTypeTyp(list)),_,_) => throw new UnsupportedOperationException("Lists of simple types aren't supported yet")

      // Make intrinsic type alias
      case (_,_,Some(SimpleTypeTyp(st: SimpleType)),_,_) if IntrinsicType.contains(st) => {
        List(mkTypeAlias(path, name, mkIntrinsicType(st)))
      }

      // Handle oneOfs
      case (_,_,_,Some(schemas),_) => {
        mkUnionTypeDef(path, name, schemas)
      }

      // Handle all or anyOfs and allOfs
      case (_,_,_,_,Some(schemas)) => {
        // TODO
//        List(mkTypeAlias(name, mkIntrinsicType(st)))
        Nil
      }

      case _ => Nil //throw new Exception("Have no idea how to define " + schema)

    }

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
  def mkTyp(path: List[String], schema: Root, defaultName: String): (Tree, List[Tree]) = {

    // If references existing schema, use that instead
    (schema.typ, schema.$ref, schema.enum, schema.oneOf, schema.multiOf) match {

      // A $ref takes precedence over everything
      case (_,Some(ref),_,_,_) => {
        val typ = mkTypeSelectPath(extractPathFromRef(path, ref))
        (typ, Nil)
      }

      // Handle array types
      case (Some(SimpleTypeTyp(SimpleTypes.Array)),_,_,_,_) => {

        val itemsSchema = schema.items match {
          case Some(ItemsRoot(s)) => s
          case Some(ItemsSchemaArray(sa)) =>
            throw new UnsupportedOperationException("SchemaArrays within items are currently unsupported: " + schema)
          case None =>
            throw new Exception("Array types must have an items property within the schema. " + schema)
        }

        val (typ, defs) = mkTyp(path, itemsSchema, defaultName)
        (inList(typ), defs)
      }

      // Make intrinsic type reference
      case (Some(SimpleTypeTyp(st: SimpleType)),_,_,_,_) if IntrinsicType.contains(st) => {
        (mkIntrinsicType(st), Nil)
      }

      // If it contains any inline definitions, then delegate to mkDef
      case (Some(SimpleTypeTyp(SimpleTypes.Object)),_,_,_,_)
           | (_,_,Some(_),_,_)
           | (_,_,_,Some(_),_)
           | (_,_,_,_,Some(_)) => {
        val defs = mkSchemaDef(path, defaultName, schema)
        val typ = tq"${ mkTypeSelectPath(path :+ defaultName) }"
        (typ, defs)
      }

      // Otherwise, assume type Any
      case _ => {
        (tq"Any", Nil)
      }

    }

  }

  /**
    * Makes a value definition (e.g. val i: Int). These are used for constructing parameter lists
    * and declaring local objects
    */
  def mkValDef(path: List[String], field: Field, optional: Boolean): (ValDef, List[Tree]) = {
    val schema = field.schema

    val (typ, defs) = mkTyp(path, schema, field.name.capitalize)

    val valDef = (if (optional) {
      q"val ${TermName(field.name)}: ${inOption(typ)} = None"
    } else {
      q"val ${TermName(field.name)}: $typ"
    }).asInstanceOf[ValDef]

    (valDef, defs)
  }

  def mkTypeAlias(path: List[String], name: String, typ: String): Tree = mkTypeAlias(path, name, Ident(TypeName(typ)))
  def mkTypeAlias(path: List[String], name: String, typ: Tree): Tree = q"type ${TypeName(name)} = $typ"

  def mkSchemaDef(path: List[String], name: String, schema: Root): List[Tree] = {

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
