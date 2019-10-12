package argus

import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._
import io.circe._
import io.circe.syntax._

package object schema {

  // -------
  // Model objects
  // -------

  type SchemaArray = List[Schema]
  type StringArray = List[String]
  type PositiveInteger = Int

  import Schema._

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

  case class Field(name: String, schema: Schema)

  implicit val FieldDecoder: Decoder[List[Field]] =
    Decoder.instance((c) => {
      c.focus.flatMap(_.asObject) match {

        // Extract each field and parse it's sub-schema as a an Root schema
        case Some(obj) => {

          val results = for {
            (name, json) <- obj.toList
            result = json.as[Schema].map(Field(name, _))
          } yield result

          // sequence() is cats magic to make List(Right(1), Right(2)) into Either(List(1,2)), used here ensure
          // that if parse errors occured within fields, those errors as propogated up
          // NB: explicit type is just to help IntelliJ realize this is valid. Not really required.
          results.sequence: Either[DecodingFailure, List[Field]]
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
  case class ItemsRoot(x: Schema) extends Items
  case class ItemsSchemaArray(x: List[Schema]) extends Items

  implicit val ItemsDecoder: Decoder[Items] =
    Decoder.instance { c =>
      (c.as[Schema].map(ItemsRoot(_))) orElse
      (c.as[SchemaArray].map(ItemsSchemaArray(_)))
    }

  implicit def ItemsEncoder: Encoder[Items] =
    Encoder.instance {
      case r: ItemsRoot => r.x.asJson
      case sa: ItemsSchemaArray => sa.x.asJson
      case t@_ => throw new Exception("Don't know typ" + t)
    }

}

