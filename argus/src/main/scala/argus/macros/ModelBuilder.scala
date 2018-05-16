package argus.macros

import scala.language.experimental.macros
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
    * Make a type from a SimpleType and an optional Format, when st is a built in type (i.e. Boolean, Int, Double,
    * String, or Null)
    */
  def mkIntrinsicType(st: SimpleType, format: Option[Format]): Tree = (st,format) match {
    case (SimpleTypes.Boolean, _) => tq"Boolean"
    case (SimpleTypes.Integer, Some(Formats.Int64)) => tq"Long"
    case (SimpleTypes.Integer, Some(Formats.Int16)) => tq"Short"
    case (SimpleTypes.Integer, Some(Formats.Int8)) => tq"Byte"
    case (SimpleTypes.Integer, _)  => tq"Int"
    case (SimpleTypes.Number, Some(Formats.Single)) => tq"Float"
    case (SimpleTypes.Number, _) => tq"Double"
    case (SimpleTypes.String, Some(Formats.Uuid)) => tq"java.util.UUID"
    case (SimpleTypes.String, Some(Formats.DateTime)) => tq"java.time.ZonedDateTime"
    case (SimpleTypes.String, _) => tq"String"
    case (SimpleTypes.Null, _) => tq"Null"
    case _ => throw new Exception("Type isn't a known intrinsic type " + st)
  }

  /**
    * Main workhorse. Creates case-classes from given fields.
    */
  def mkCaseClassDef(path: List[String], name: String, fields: List[Field],
                     requiredFields: Option[List[String]]): (Tree, List[Tree]) = {

    // Build val defs for each field in case class, keeping track of new class defs created along the way (for nested
    // types
    val (params, fieldDefs) = fields.foldLeft((List[ValDef](), List[Tree]())) { case ((valDefs, defDefs), field) =>
      // If required, then don't wrap in Option
      val optional = !requiredFields.getOrElse(Nil).contains(field.name)

      val enums = field.schema.enum.getOrElse(List())
      val defval: Option[Select] = if (enums.length == 1) 
          Some(enumName(name, field.name, enums.head))
        else 
          None

      val (valDef, defDef) = mkValDef(path :+ name, field, optional, defval)
      (valDefs :+ valDef, defDefs ++ defDef)
    }

    val typ = mkTypeSelectPath(path :+ name)
    val ccDef = q"""case class ${ TypeName(name) } (..$params)"""

    val defs = if (fieldDefs.isEmpty)
      ccDef :: Nil
    else
      ccDef :: q"""object ${TermName(name)} { ..$fieldDefs }""" :: Nil

    (typ, defs)
  }

  /**
    * Makes a representation of an enumeration type. We model this as a "sum type" in scala, where each enum is an object
    * that inherits from the base trait. The base trait is sealed so that it can be used for pattern matching.
    *
    * @param baseName Name of the enum (becomes the name of the base trait)
    * @param enum List of all possible enum values (encoded as a string containing their json representation)
    * @return List[Tree] containing all the definitions
    */
  def mkEnumDef(path: List[String], baseName: String, enum: List[String]): (Tree, List[Tree]) = {
    val baseTyp = TypeName(baseName)
    val baseDef = q"@enum sealed trait $baseTyp extends scala.Product with scala.Serializable { def json: String }"

    val memberDefs = enum.map { m =>
      val name = TermName(nameFromJson(m))
      q"case object $name extends $baseTyp { val json: String = $m }"
    }

    val typ = mkTypeSelectPath(path :+ baseTyp.toString)
    val defs = baseDef :: q"""object ${TermName(baseName + "Enums")} { ..$memberDefs }""" :: Nil

    (typ, defs)
  }

  /**
    * The fully-qualified name of an enum case.
    */
  def enumName(ownerName: String, fieldName: String, caseName: String): Select = {
    Select(Select(Ident(TermName(ownerName)), TermName(fieldName.capitalize + "Enums")), TermName(nameFromJson(caseName)))
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
  def mkUnionTypeDef(path: List[String], baseName: String, schemas: List[Root]): (Tree, List[Tree]) = {
    val baseTyp = TypeName(baseName + "Union")
    val baseDef = q"@union sealed trait $baseTyp extends scala.Product with scala.Serializable"

    val (memberDefs, defDefs) = schemas.zipWithIndex.foldLeft(List[Tree](), List[Tree]()) {
      case((md, dd), (schema, i)) =>

        // Hopefully we don't have to use this, but if one of the schema's is an anonymous type then we don't have much
        // choice
        val defaultName = baseName + (i + 1).toString
        val (typ, defs) = mkType(path, schema, defaultName)

        // E.g. FooInt
        val name = TypeName(baseName + nameFromType(typ, false))

        val memberDef = q"case class $name(x: $typ) extends $baseTyp"
        (md :+ memberDef,  dd ++ defs)

    }

    val typ = mkTypeSelectPath(path :+ baseTyp.toString)

    (typ, baseDef :: memberDefs ++ defDefs)
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
    */
  def mkDef(path: List[String], name: String, schema: Root): (Tree, List[Tree]) = {

    (schema.$ref, schema.enum, schema.typ,  schema.oneOf, schema.multiOf) match {

      // Refs
      case (Some(ref),_,_,_,_) => {
        val toType = mkTypeSelectPath(extractPathFromRef(path.headOption, ref))
        mkTypeAlias(path, name, toType)
      }

      // Enums
      case (_,Some(enum),_,_,_) => {
        mkEnumDef(path, name, enum)
      }

      // Object (which defines a case-class)
      case (_,_,Some(SimpleTypeTyp(SimpleTypes.Object)),_,_) => {
        mkCaseClassDef(path, name, schema.properties.get, schema.required)
      }

      // Array, create type alias to List of type defined by schema.items (which itself is a schema)
      case (_,_,Some(SimpleTypeTyp(SimpleTypes.Array)),_,_) => {
        val (toType, arrayDefs) = mkType(path, schema, name + "Item")
        val (typ, aliasDefs) = mkTypeAlias(path, name, toType)
        (typ, arrayDefs ++ aliasDefs)
      }

      // Alias to an intrinsic type
      case (_,_,Some(SimpleTypeTyp(st: SimpleType)),_,_) if IntrinsicType.contains(st) => {
        mkTypeAlias(path, name, mkIntrinsicType(st, schema.format))
      }

      // OneOfs (aka Union types)
      case (_,_,_,Some(schemas),_) => {
        mkUnionTypeDef(path, name, schemas)
      }

      // AnyOfs and AllOfs (aka List of type)
      // TODO
      case (_,_,_,_,Some(schemas)) => {
        // List(mkTypeAlias(name, mkIntrinsicType(st)))
        (EmptyTree, Nil)
      }

      case (_,_,Some(ListSimpleTypeTyp(list)),_,_) => throw new UnsupportedOperationException("Lists of simple types aren't supported yet")

      // Nothing in the schema? Then nothing to define
      case _ => {
        (EmptyTree, Nil)
      }

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
  def mkType(path: List[String], schema: Root, defaultName: String): (Tree, List[Tree]) = {

    // Types are a bit strange. They are type definitions and schemas. We extract any inner /definitions
    // and embed those
    val (_, defDefs) = mkSchemaDef(defaultName, schema.justDefinitions, path)

    // If references existing schema, use that instead
    (schema.typ, schema.$ref, schema.enum, schema.oneOf, schema.multiOf) match {

      // A $ref takes precedence over everything
      case (_,Some(ref),_,_,_) => {

        // XXX This is a bit broken. It works if the type exactly matches the reference string. But that's not
        // always the case (e.g. union types have "union" attached to end of their name). Ideally we'd maintain a mapping
        // of refs -> types, and then resolve references in some kind of linking stage. Leaving this for now, but will
        // need to be fixed at some point.
        val typ = mkTypeSelectPath(extractPathFromRef(path.headOption, ref))
        (typ, defDefs)
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

        val (typ, itemDefs) = mkType(path, itemsSchema, defaultName)
        (inList(typ), defDefs ++ itemDefs)
      }

      // Make intrinsic type reference
      case (Some(SimpleTypeTyp(st: SimpleType)),_,_,_,_) if IntrinsicType.contains(st) => {
        (mkIntrinsicType(st, schema.format), defDefs)
      }

      // If it contains inline definitions, then delegate to mkSchema, and name type after the parameter name.
      case (Some(SimpleTypeTyp(SimpleTypes.Object)),_,_,_,_)
           | (_,_,Some(_),_,_)
           | (_,_,_,Some(_),_)
           | (_,_,_,_,Some(_)) => {

        // NB: We ignore defDefs here since we're re-calling mkSchema
        mkSchemaDef(defaultName, schema, path)
      }

      // If not type info specified then we have no option but to make it a map of strings (field names) to anys (values)
      // this is going to requiring runtime casts to do anything useful with them, but what else is can we do?
      case _ => {
        val (typ, anyDef) = mkAnyWrapper(path, defaultName)
        (typ, defDefs ++ anyDef)
      }

    }

  }

  /**
    * If there's no schema specified what do we do? It seems this leaves the json open to be anything. The best we can do
    * is make it an Any type, but to stop things getting too far out of control we wrap it in a type so that we can
    * maintain some control over the encoding/decodng (and allow for customization).
    *
    * @param path The package path to where this is type is defined.
    * @param name The name of the wrapper class.
    */
  def mkAnyWrapper(path: List[String], name: String): (Tree, List[Tree]) = {
    val typ = mkTypeSelectPath(path :+ name)
    (typ, q"case class ${TypeName(name)}(x: Any)" :: Nil)
  }

  /**
    * Makes a value definition (e.g. val i: Int). These are used for constructing parameter lists
    * and declaring local objects
    */
  def mkValDef(path: List[String], field: Field, optional: Boolean, defval: Option[Select] = None): (ValDef, List[Tree]) = {
    val schema = field.schema

    val (typ, defs) = mkType(path, schema, field.name.capitalize)

    val valDef = (if (optional) {
      val dval = defval.map({ x => q"Some(${x})" }).getOrElse(q"None")
      q"val ${TermName(field.name)}: ${inOption(typ)} = ${dval}"
    } else {
      defval match {
        case None => q"val ${TermName(field.name)}: ${typ}"
        case Some(dval) => q"val ${TermName(field.name)}: ${typ} = ${dval}"
      }
    }).asInstanceOf[ValDef]

    (valDef, defs)
  }

  /**
    * Creates type alias definitions
    * @return A tuple containing the created type, and a type alias definition
    */
  def mkTypeAlias(path: List[String], name: String, toType: Tree): (Tree, List[Tree]) = {
    (mkTypeSelectPath(path :+ name), q"type ${TypeName(name)} = $toType" :: Nil)
  }

  /**
    * Makes all definitions required to define the given schema
    * @param name The name of the root type that represents this schema
    * @param schema The schema to generate from.
    * @param path A package path for where this is defined. Defaults to Nil.
    * @return A tuple containing the type of the root element that is generated, and all definitions required to support it
    */
  def mkSchemaDef(name: String, schema: Root, path: List[String] = Nil): (Tree, List[Tree]) = {

    // Make definitions
    val fieldDefs = for {
      fields <- schema.definitions.toList
      field <- fields
      (_, defDefs) = mkSchemaDef(field.name.capitalize, field.schema, path)
      defDef <- defDefs
    } yield defDef

    // Make root
    val (typ, rootDefs) = mkDef(path, name, schema)

    (typ, fieldDefs ++ rootDefs)
  }


}
