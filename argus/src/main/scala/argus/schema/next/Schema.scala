package argus.schema.next

//import java.io.InputStream
//import scala.io.Source

import io.circe._
import io.circe.syntax._

import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._

// ---
// Schema Types
// ---

sealed trait Schema extends Product with Serializable

object Schema {

  implicit val SchemaDecoder: Decoder[Schema] =
    Decoder.instance { c =>
      c.as[RefSchema] orElse
      c.as[NullSchema.type] orElse
      c.as[BooleanSchema] orElse
      c.as[StringSchema] orElse
      c.as[ObjectSchema]orElse
      c.as[ArraySchema] orElse
      c.as[IntegerSchema]orElse
      c.as[NumberSchema]
    }

  implicit def ItemsEncoder: Encoder[Schema] =
    Encoder.instance {
      case rs: RefSchema => rs.asJson
      case ns: NullSchema.type => ns.asJson
      case bs: BooleanSchema => bs.asJson
      case ss: StringSchema => ss.asJson
      case os: ObjectSchema => os.asJson
      case as: ArraySchema => as.asJson
      case is: IntegerSchema => is.asJson
      case ns: NumberSchema => ns.asJson
      case i@_ => throw new Exception("Don't know how to encode" + i)
    }
}

case class RefSchema($ref: String) extends Schema

object RefSchema {

  implicit val refSchemaDecoder: Decoder[RefSchema] =
    Decoder
      .instance((c) => for {
        $ref <- c.get[String]("$ref")
      } yield RefSchema($ref))
      .validate(
        (c) => c.keys.map(_.toSet) == Some(Set("$ref")),
        "$ref can only have that as an attribute"
      )

  implicit val refSchemaEncoder: Encoder[RefSchema] =
    Encoder.instance((r: RefSchema) => Json.obj(
      "$ref" -> r.$ref.asJson
    ))

}

case object NullSchema extends Schema {

  implicit val nullSchemaDecoder: Decoder[NullSchema.type] =
    Decoder
      .instance((c) => for {
        typ <- c.get[String]("type")
      } yield NullSchema)
      .validate(
        (c) => c.get[String]("type") == "null",
      "Type attribute isn't 'null'"
      )

  implicit val nullSchemaEncoder: Encoder[NullSchema.type] =
    Encoder.instance((r: NullSchema.type) => Json.obj(
      "type" -> "null".asJson
    ))

}

// ---
// Instance Types
// ---

sealed trait InstanceSchema extends Schema {
  // Metadata
  val $schema: Option[String]
  val id: Option[String]
  val title: Option[String]
  val description: Option[String]
  val definitions: Option[List[NamedSchema]]

  // Branch terms
  val oneOf: Option[SchemaList]
  val anyOf: Option[SchemaList]
  val allOf: Option[SchemaList]
  val not: Option[Schema]

//  def toJson = this.asJson
//  def toJsonString = this.asJson.pretty(printer)
//  val printer = Printer.spaces2.copy(dropNullValues = true)

//  def justDefinitions = Schema(definitions=this.definitions)

  // Convenience method since any/all of are treated roughly the same by the generated code
//  def multiOf = anyOf.orElse(allOf)

}

object InstanceSchema {

  def decodeFields(c: HCursor) = for {
    $schema     <- c.downField("$schema").as[Option[String]]
    id          <- c.downField("id").as[Option[String]]
    title       <- c.downField("title").as[Option[String]]
    description <- c.downField("description").as[Option[String]]
    definitions <- c.downField("definitions").as[Option[List[NamedSchema]]]
    oneOf       <- c.downField("oneOf").as[Option[SchemaList]]
    anyOf       <- c.downField("anyOf").as[Option[SchemaList]]
    allOf       <- c.downField("allOf").as[Option[SchemaList]]
    not         <- c.downField("not").as[Option[Schema]]
  } yield ($schema, id, title, description, definitions, oneOf, anyOf, allOf, not)

  def encodeFields(r: InstanceSchema) = List(
    "$schema"     -> r.$schema.asJson,
    "id"          -> r.id.asJson,
    "title"       -> r.title.asJson,
    "description" -> r.description.asJson,
    "definitions" -> r.definitions.asJson,
    "oneOf"       -> r.oneOf.asJson,
    "anyOf"       -> r.anyOf.asJson,
    "allOf"       -> r.allOf.asJson,
    "not"         -> r.not.asJson
  )

}

case class BooleanSchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                         description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                         oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None)
  extends InstanceSchema

object BooleanSchema {

  implicit val booleanSchemaDecoder: Decoder[BooleanSchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
      } yield BooleanSchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not))
      .validate(
        (c) => c.get[String]("type") == "boolean",
        "Type attribute isn't 'boolean'"
      )

  implicit val booleanSchemaEncoder: Encoder[BooleanSchema] =
    Encoder.instance((r: BooleanSchema) => Json.fromFields( InstanceSchema.encodeFields(r) ))

}

case class StringSchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                        description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                        oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None,
                        maxLength: Option[Int] = None, minLength: Option[Int] = None, pattern: Option[String] = None)
  extends InstanceSchema

object StringSchema {

  implicit val stringSchemaDecoder: Decoder[StringSchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
        maxLength <- c.downField("maxLength").as[Option[Int]]
        minLength <- c.downField("minLength").as[Option[Int]]
        pattern   <- c.downField("pattern").as[Option[String]]
      } yield StringSchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not, maxLength, minLength, pattern))
      .validate(
        (c) => c.get[String]("type") == "string",
        "Type attribute isn't 'string'"
      )

  implicit val stringSchemaEncoder: Encoder[StringSchema] =
    Encoder.instance((r: StringSchema) => Json.fromFields(InstanceSchema.encodeFields(r) ++ List(
      "maxLength" -> r.maxLength.asJson,
      "minLength" -> r.minLength.asJson,
      "pattern"   -> r.pattern.asJson
    )))

}

case class ObjectSchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                        description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                        oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None,
                        properties: Option[List[NamedSchema]] = None, required: Option[StringArray] = None, propertyNames: Option[Schema] = None,
                        additionalProperties: Option[Schema] = None, patternProperties: Option[Schema] = None)
  extends InstanceSchema

object ObjectSchema {

  implicit val objectSchemaDecoder: Decoder[ObjectSchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
        properties           <- c.downField("properties").as[Option[List[NamedSchema]]]
        required             <- c.downField("required").as[Option[StringArray]]
        propertyNames        <- c.downField("propertyNames").as[Option[Schema]]
        additionalProperties <- c.downField("additionalProperties").as[Option[Schema]]
        patternProperties    <- c.downField("patternProperties").as[Option[Schema]]
      } yield ObjectSchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not,
        properties, required, propertyNames, additionalProperties, patternProperties))
      .validate(
        (c) => c.get[String]("type") == "object",
        "Type attribute isn't 'object'"
      )

  implicit val objectSchemaEncoder: Encoder[ObjectSchema] =
    Encoder.instance((r: ObjectSchema) => Json.fromFields(InstanceSchema.encodeFields(r) ++ List(
      "properties"           -> r.properties.asJson,
      "required"             -> r.required.asJson,
      "propertyNames"        -> r.propertyNames.asJson,
      "additionalProperties" -> r.additionalProperties.asJson,
      "patternProperties"    -> r.patternProperties.asJson
    )))

}

case class ArraySchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                       description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                       oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None,
                       items: Option[Items] = None, additonalItems: Option[Schema] = None, maxItems: Option[Int] = None, minItems: Option[Int] = None,
                       uniqueItems: Option[Boolean] = None, contains: Option[Schema] = None)
  extends InstanceSchema

object ArraySchema {

  implicit val arraySchemaDecoder: Decoder[ArraySchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
        items          <- c.downField("items").as[Option[Items]]
        additonalItems <- c.downField("additonalItems").as[Option[Schema]]
        maxItems       <- c.downField("maxItems").as[Option[Int]]
        minItems       <- c.downField("minItems").as[Option[Int]]
        uniqueItems    <- c.downField("uniqueItems").as[Option[Boolean]]
        contains       <- c.downField("contains").as[Option[Schema]]
      } yield ArraySchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not,
        items, additonalItems, maxItems, minItems, uniqueItems, contains))
      .validate(
        (c) => c.get[String]("type") == "array",
        "Type attribute isn't 'array'"
      )

  implicit val arraySchemaEncoder: Encoder[ArraySchema] =
    Encoder.instance((r: ArraySchema) => Json.fromFields(InstanceSchema.encodeFields(r) ++ List(
      "items"          -> r.items.asJson,
      "additonalItems" -> r.additonalItems.asJson,
      "maxItems"       -> r.maxItems.asJson,
      "minItems"       -> r.minItems.asJson,
      "uniqueItems"    -> r.uniqueItems.asJson,
      "contains"       -> r.contains.asJson
    )))

}

// ---
// Numeric Types
// ---

sealed trait NumericSchema extends InstanceSchema {
  val multipleOf: Option[Double]
  val maximum: Option[Double]
  val exclusiveMaximum: Option[Double]
  val minimum: Option[Double]
  val exclusiveMinimum: Option[Double]
}

object NumericSchema {

  def decodeFields(c: HCursor) = for {
    multipleOf       <- c.downField("multipleOf").as[Option[Double]]
    maximum          <- c.downField("maximum").as[Option[Double]]
    exclusiveMaximum <- c.downField("exclusiveMaximum").as[Option[Double]]
    minimum          <- c.downField("minimum").as[Option[Double]]
    exclusiveMinimum <- c.downField("exclusiveMinimum").as[Option[Double]]
  } yield (multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum)

  def encodeFields(r: NumericSchema) = List(
    "multipleOf"       -> r.multipleOf.asJson,
    "maximum"          -> r.maximum.asJson,
    "exclusiveMaximum" -> r.exclusiveMaximum.asJson,
    "minimum"          -> r.minimum.asJson,
    "exclusiveMinimum" -> r.exclusiveMinimum.asJson,
  )

}

case class IntegerSchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                         description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                         oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None,
                         multipleOf: Option[Double] = None, maximum: Option[Double] = None, exclusiveMaximum: Option[Double] = None,
                         minimum: Option[Double] = None, exclusiveMinimum: Option[Double] = None)
  extends NumericSchema

object IntegerSchema {

  implicit val integerSchemaDecoder: Decoder[IntegerSchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
        t2 <- NumericSchema.decodeFields(c)
        (multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum) = t2
      } yield IntegerSchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not,
        multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum))
      .validate(
        (c) => c.get[String]("type") == "integer",
        "Type attribute isn't 'number'"
      )

  implicit val integerSchemaEncoder: Encoder[IntegerSchema] =
    Encoder.instance((r: IntegerSchema) => Json.fromFields(
      InstanceSchema.encodeFields(r) ++ NumericSchema.encodeFields(r)
    ))

}

case class NumberSchema($schema: Option[String] = None, id: Option[String] = None, title: Option[String] = None,
                        description: Option[String] = None, definitions: Option[List[NamedSchema]] = None,
                        oneOf: Option[SchemaList] = None, anyOf: Option[SchemaList] = None, allOf: Option[SchemaList] = None, not: Option[Schema] = None,
                        multipleOf: Option[Double] = None, maximum: Option[Double] = None, exclusiveMaximum: Option[Double] = None,
                        minimum: Option[Double] = None, exclusiveMinimum: Option[Double] = None)
  extends NumericSchema

object NumberSchema {

  implicit val numberSchemaDecoder: Decoder[NumberSchema] =
    Decoder
      .instance((c) => for {
        t <- InstanceSchema.decodeFields(c)
        (schema, id, title, description, definitions, oneOf, anyOf, allOf, not) = t
        t2 <- NumericSchema.decodeFields(c)
        (multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum) = t2
      } yield NumberSchema(schema, id, title, description, definitions, oneOf, anyOf, allOf, not,
        multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum))
      .validate(
        (c) => c.get[String]("type") == "number",
        "Type attribute isn't 'number'"
      )

  implicit val numberSchemaEncoder: Encoder[NumberSchema] =
    Encoder.instance((r: NumberSchema) => Json.fromFields(
      InstanceSchema.encodeFields(r) ++ NumericSchema.encodeFields(r)
    ))

}



