package argus.schema

import java.io.InputStream

import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._
import io.circe._
import io.circe.syntax._

import scala.io.Source

object Schema {

  def fromInputStream(is: InputStream): Root = {
    if (is == null) throw new Exception("Input stream is null")
    fromJson(Source.fromInputStream(is).getLines.mkString("\n"))
  }

  def fromURL(url: String): Root = {
    JsonNumber
    JsonObject
    fromJson(Source.fromURL(url).getLines.mkString("\n"))
  }

  def fromResource(path: String) = fromInputStream(getClass.getResourceAsStream(path))

  def fromJson(str: String): Root = {
    val result = for {
      json <- parser.parse(str)
      root <- json.as[Root]
    } yield root

    result match {
      case Right(root) => root
      case Left(failure) => throw new Exception("Error parsing schema: " + failure.toString)
    }
  }

  /**
    * Convenience method for creating a schema from a simple type (e.g. { "type" : "string" }
    */
  def schemaFromSimpleType(st: SimpleType) = Root(typ=Some(SimpleTypeTyp(st)))

  /**
    * Convenience method for creating schemas that represent an object (based on given fields)
    */
  def schemaFromFields(fields: List[Field]) = Root(typ=Some(SimpleTypeTyp(SimpleTypes.Object)), properties=Some(fields))

  /**
    * Convenience method for creating schemas that reference another schema
    */
  def schemaFromRef(ref: String) = Root($ref = Some(ref))

  /**
    * Convenience method for creating schemas from enums
    * @param enum A list if items to enum (encoded as json strings)
    */
  def schemaFromEnum(enum: List[String]) = Root(enum = Some(enum))

  /**
    * Convenience method for creating schemas from an array
    * @param schema A schema that defines the type of the array
    */
  def schemaFromArray(schema: Root) = Root(typ=Some(SimpleTypeTyp(SimpleTypes.Array)), items=Some(ItemsRoot(schema)))

  /**
    * Convenience method for creating schemas based on a union type of the listed schemas (aka oneOf)
    */
  def schemaFromUnionType(schemas: List[Root]) = Root(oneOf=Some(schemas))

  // -------
  // Model objects
  // -------

  type PositiveInteger = Int
  type SchemaArray = List[Root]
  type StringArray = List[String]

  case class Root
    ($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None, description: Option[String] = None,
    definitions: Option[List[Field]] = None, properties: Option[List[Field]] = None,
    typ: Option[Typ] = None, enum: Option[List[String]] = None,
    oneOf: Option[SchemaArray] = None, anyOf: Option[SchemaArray] = None, allOf: Option[SchemaArray] = None,
    not: Option[Root] = None, required: Option[StringArray] = None, items: Option[Items] = None, format: Option[String] = None,
    minimum: Option[Double] = None, maximum: Option[Double] = None, exclusiveMinimum: Option[Boolean] = None, exclusiveMaximum: Option[Boolean] = None,
    $ref: Option[String] = None) {

    def toJson = this.asJson
    def toJsonString = this.asJson.pretty(printer)
    val printer = Printer.spaces2.copy(dropNullKeys = true)

    // Convenience method since any/all of are treated roughly the same by the generated code
    def multiOf = anyOf.orElse(allOf)

    def justDefinitions = Root(definitions=this.definitions)
  }

  // Useful for dealing with enums, which we leave as raw Json, but because we don't want to expose our Json implementation
  // in the external API we encode as strings
  private def toStringList(jl: Option[List[Json]]): Option[List[String]] = jl.map(_.map(_.noSpaces))
  private def toJsonList(sl: Option[List[String]]): Option[List[Json]] = sl.map(_.map(s => parser.parse(s).toOption.get))

  implicit val RootDecoder: Decoder[Root] =
    Decoder.instance((c) => for {
      $schema <- c.downField("$schema").as[Option[String]]
      id <- c.downField("id").as[Option[String]]
      title <- c.downField("title").as[Option[String]]
      description <- c.downField("description").as[Option[String]]
      definitions <- c.downField("definitions").as[Option[List[Field]]]
      properties <- c.downField("properties").as[Option[List[Field]]]
      typ <- c.downField("type").as[Option[Typ]]
      enum <- c.downField("enum").as[Option[List[Json]]].map(toStringList)
      oneOf <- c.downField("oneOf").as[Option[SchemaArray]]
      anyOf <- c.downField("anyOf").as[Option[SchemaArray]]
      allOf <- c.downField("allOf").as[Option[SchemaArray]]
      not <- c.downField("not").as[Option[Root]]
      required <- c.downField("required").as[Option[StringArray]]
      items <- c.downField("items").as[Option[Items]]
      format <- c.downField("format").as[Option[String]]
      minimum <- c.downField("minimum").as[Option[Double]]
      maximum <- c.downField("maximum").as[Option[Double]]
      exclusiveMinimum <- c.downField("exclusiveMinimum").as[Option[Boolean]]
      exclusiveMaximum <- c.downField("exclusiveMaximum").as[Option[Boolean]]
      $ref <- c.downField("$ref").as[Option[String]]
    } yield Root($schema, id, title, description, definitions, properties, typ, enum, oneOf, anyOf, allOf, not, required,
                 items, format, minimum, maximum, exclusiveMinimum, exclusiveMaximum, $ref))

  implicit val RootEncoder: Encoder[Root] =
    Encoder.instance((r: Root) => Json.obj(
      "$schema" -> r.$schema.asJson,
      "id" -> r.id.asJson,
      "title" -> r.title.asJson,
      "description" -> r.description.asJson,
      "definitions" -> r.definitions.asJson,
      "properties" -> r.properties.asJson,
      "type" -> r.typ.asJson,
      "enum" -> toJsonList(r.enum).asJson,
      "oneOf" -> r.oneOf.asJson,
      "anyOf" -> r.anyOf.asJson,
      "allOf" -> r.allOf.asJson,
      "not" -> r.not.asJson,
      "required" -> r.required.asJson,
      "items" -> r.items.asJson,
      "format" -> r.format.asJson,
      "minimum" -> r.minimum.asJson,
      "maximum" -> r.maximum.asJson,
      "exclusiveMinimum" -> r.exclusiveMinimum.asJson,
      "exclusiveMaximum" -> r.exclusiveMaximum.asJson,
      "$ref" -> r.$ref.asJson
    ))

  sealed trait Typ
  case class SimpleTypeTyp(x: SimpleType) extends Typ
  case class ListSimpleTypeTyp(x: List[SimpleType]) extends Typ

  implicit val TypDecoder: Decoder[Typ] =
    Decoder.instance { c =>
      (c.as[List[SimpleType]].map(ListSimpleTypeTyp(_))) orElse
      (c.as[SimpleType].map(SimpleTypeTyp(_)))
    }

  implicit def TypEncoder: Encoder[Typ] =
    Encoder.instance {
      case st: SimpleTypeTyp => st.x.asJson
      case lst: ListSimpleTypeTyp => lst.x.asJson
      case t@_ => throw new Exception("Don't know typ" + t)
    }

  sealed trait SimpleType { def name: String }
  object SimpleTypes {
    case object Array extends SimpleType { val name = "array" }
    case object Boolean extends SimpleType { val name = "boolean" }
    case object Integer extends SimpleType { val name = "integer" }
    case object Null extends SimpleType { val name = "null" }
    case object Number extends SimpleType { val name = "number" }
    case object Object extends SimpleType { val name = "object" }
    case object String extends SimpleType { val name = "string" }
  }

  implicit def SimpleTypeDecoder: Decoder[SimpleType] =
    Decoder.instance((c) => for {
      str <- c.as[String]
      typ <- str match {
        case "array" => Either.right(SimpleTypes.Array)
        case "boolean" => Either.right(SimpleTypes.Boolean)
        case "integer" => Either.right(SimpleTypes.Integer)
        case "null" => Either.right(SimpleTypes.Null)
        case "number" => Either.right(SimpleTypes.Number)
        case "object" => Either.right(SimpleTypes.Object)
        case "string" => Either.right(SimpleTypes.String)
        case t@_ => Either.left(DecodingFailure("Don't know simple type " + t, c.history))
      }
    } yield typ)

  implicit def SimpleTypeEncoder: Encoder[SimpleType] = Encoder.instance(_.name.asJson)

  case class Field(name: String, schema: Root)

  implicit val FieldDecoder: Decoder[List[Field]] =
    Decoder.instance((c) => {
      c.focus.flatMap(_.asObject) match {

        // Extract each field and parse it's sub-schema as a an Root schema
        case Some(obj) => {

          val results = for {
            (name, json) <- obj.toList
            result = json.as[Root].map(Field(name, _))
          } yield result

          // sequence() is cats magic to make List(Right(1), Right(2)) into Either(List(1,2)), used here ensure
          // that if parse errors occured within fields, those errors as propogated up
          // NB: explicit type is just to help IntelliJ realize this is valid. Not really required.
          results.sequenceU: Either[DecodingFailure, List[Field]]
        }

        // Properties isn't an object?!?
        case _ => Either.left(DecodingFailure("Properties isn't an object?", c.history))
      }

    })

  implicit val FieldEncoder: Encoder[List[Field]] =
    Encoder.instance((lf: List[Field]) => {
      val fieldMap = lf.map { x => (x.name, x.schema.asJson) }
      Json.obj(fieldMap: _*)
    })

  sealed trait Items
  case class ItemsRoot(x: Root) extends Items
  case class ItemsSchemaArray(x: List[Root]) extends Items

  implicit val ItemsDecoder: Decoder[Items] =
    Decoder.instance { c =>
      (c.as[Root].map(ItemsRoot(_))) orElse
      (c.as[SchemaArray].map(ItemsSchemaArray(_)))
    }

  implicit def ItemsEncoder: Encoder[Items] =
    Encoder.instance {
      case r: ItemsRoot => r.x.asJson
      case sa: ItemsSchemaArray => sa.x.asJson
      case t@_ => throw new Exception("Don't know typ" + t)
    }

}

