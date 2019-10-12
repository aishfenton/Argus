package argus.schema

import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._
import io.circe._
import io.circe.syntax._

package object next {

  type StringArray = List[String]
  type SchemaList = List[next.Schema]

  case class Ref(path: List[String])

  object Ref {
    def fromString(path: String): Ref = {
      Ref( path.split(".").toList )
    }
  }

  case class NamedSchema(name: String, schema: next.Schema)

  implicit val NamedSchemaDecoder: Decoder[List[NamedSchema]] =
    Decoder.instance((c) => {
      c.focus.flatMap(_.asObject) match {

        // Extract each field and parse it's sub-schema as a an Root schema
        case Some(obj) => {

          val results = for {
            (name, json) <- obj.toList
            result = json.as[next.Schema].map(NamedSchema(name, _))
          } yield result

          // sequence() is cats magic to make List(Right(1), Right(2)) into Either(List(1,2)), used here ensure
          // that if parse errors occured within fields, those errors as propogated up
          // NB: explicit type is just to help IntelliJ realize this is valid. Not really required.
          results.sequence: Either[DecodingFailure, List[NamedSchema]]
        }

        // Properties isn't an object?!?
        case _ => Either.left(DecodingFailure("Properties isn't an object?", c.history))
      }

    })

  implicit val NamedSchemaEncoder: Encoder[List[NamedSchema]] =
    Encoder.instance((lf: List[NamedSchema]) => {
      val fieldMap = lf.map { x => (x.name, x.schema.asJson) }
      Json.obj(fieldMap: _*)
    })


  sealed trait Items

  object Items {

    implicit val ItemsDecoder: Decoder[Items] =
      Decoder.instance { c =>
        (c.as[next.Schema].map(ItemsSchema(_))) orElse
          (c.as[SchemaList].map(ItemsSchemaList(_)))
      }

    implicit def ItemsEncoder: Encoder[Items] =
      Encoder.instance {
        case r: ItemsSchema => r.x.asJson
        case sa: ItemsSchemaList => sa.x.asJson
        case t@_ => throw new Exception("Don't know typ" + t)
      }

  }

  case class ItemsSchema(x: next.Schema) extends Items
  case class ItemsSchemaList(x: List[next.Schema]) extends Items
}
