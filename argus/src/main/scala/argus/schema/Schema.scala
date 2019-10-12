package argus.schema

import java.io.InputStream
import scala.io.Source

import io.circe._
import io.circe.syntax._

import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._

case class Schema(


  // Schema metadata
  $schema: Option[String] = None,
  id: Option[String] = None,
  title: Option[String] = None,
  description: Option[String] = None,

  // Schema defs
  definitions: Option[List[Field]] = None,

  // Object validiation keywords
  properties: Option[List[Field]] = None,
  typ: Option[Typ] = None, enum: Option[List[String]] = None,

  oneOf: Option[SchemaArray] = None, anyOf: Option[SchemaArray] = None,
  allOf: Option[SchemaArray] = None,
  not: Option[Schema] = None,

  required: Option[StringArray] = None,
  items: Option[Items] = None,
  format: Option[String] = None,
  minimum: Option[Double] = None,
  maximum: Option[Double] = None,
  exclusiveMinimum: Option[Boolean] = None,
  exclusiveMaximum: Option[Boolean] = None,
  $ref: Option[String] = None) {

  def toJson = this.asJson
  def toJsonString = this.asJson.pretty(printer)
  val printer = Printer.spaces2.copy(dropNullValues = true)

  // Convenience method since any/all of are treated roughly the same by the generated code
  def multiOf = anyOf.orElse(allOf)

  def justDefinitions = Schema(definitions=this.definitions)
}

object Schema {

  def fromInputStream(is: InputStream): Schema = {
    if (is == null) throw new Exception("Input stream is null")
    fromJson(Source.fromInputStream(is).getLines.mkString("\n"))
  }

  def fromURL(url: String): Schema = {
    JsonNumber
    JsonObject
    fromJson(Source.fromURL(url).getLines.mkString("\n"))
  }

  def fromResource(path: String) = fromInputStream(getClass.getResourceAsStream(path))

  def fromJson(str: String): Schema = {
    val result = for {
      json <- parser.parse(str)
      root <- json.as[Schema]
    } yield root

    result match {
      case Right(root) => root
      case Left(failure) => throw new Exception("Error parsing schema: " + failure.toString)
    }
  }

  /**
    * Convenience method for creating a schema from a simple type (e.g. { "type" : "string" }
    */
  def schemaFromSimpleType(st: SimpleType) = Schema(typ=Some(SimpleTypeTyp(st)))

  /**
    * Convenience method for creating schemas that represent an object (based on given fields)
    */
  def schemaFromFields(fields: List[Field]) = Schema(typ=Some(SimpleTypeTyp(SimpleTypes.Object)), properties=Some(fields))

  /**
    * Convenience method for creating schemas that reference another schema
    */
  def schemaFromRef(ref: String) = Schema($ref = Some(ref))

  /**
    * Convenience method for creating schemas from enums
    * @param enum A list if items to enum (encoded as json strings)
    */
  def schemaFromEnum(enum: List[String]) = Schema(enum = Some(enum))

  /**
    * Convenience method for creating schemas from an array
    * @param schema A schema that defines the type of the array
    */
  def schemaFromArray(schema: Schema) = Schema(typ=Some(SimpleTypeTyp(SimpleTypes.Array)), items=Some(ItemsRoot(schema)))

  /**
    * Convenience method for creating schemas based on a union type of the listed schemas (aka oneOf)
    */
  def schemaFromUnionType(schemas: List[Schema]) = Schema(oneOf=Some(schemas))

  // Useful for dealing with enums, which we leave as raw Json, but because we don't want to expose our Json implementation
  // in the external API we encode as strings
  private def toStringList(jl: Option[List[Json]]): Option[List[String]] = jl.map(_.map(_.noSpaces))
  private def toJsonList(sl: Option[List[String]]): Option[List[Json]] = sl.map(_.map(s => parser.parse(s).toOption.get))

  implicit val schemaDecoder: Decoder[Schema] =
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
      not <- c.downField("not").as[Option[Schema]]
      required <- c.downField("required").as[Option[StringArray]]
      items <- c.downField("items").as[Option[Items]]
      format <- c.downField("format").as[Option[String]]
      minimum <- c.downField("minimum").as[Option[Double]]
      maximum <- c.downField("maximum").as[Option[Double]]
      exclusiveMinimum <- c.downField("exclusiveMinimum").as[Option[Boolean]]
      exclusiveMaximum <- c.downField("exclusiveMaximum").as[Option[Boolean]]
      $ref <- c.downField("$ref").as[Option[String]]
    } yield Schema($schema, id, title, description, definitions, properties, typ, enum, oneOf, anyOf, allOf, not, required,
      items, format, minimum, maximum, exclusiveMinimum, exclusiveMaximum, $ref))

  implicit val schemaEncoder: Encoder[Schema] =
    Encoder.instance((r: Schema) => Json.obj(
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


}

