package argus

import io.circe._
import io.circe.syntax._
import cats.data.Xor
import cats.implicits._

object Schema {
  import UnionType._

  type TypEv[-X] = (SimpleType Or List[SimpleType])#check[X]

  def parse(schema: String) = {
    val result = for {
      json <- parser.parse(schema)
      root <- json.as[Root[_]]
    } yield root

    result match {
      case Xor.Right(root) => root
      case Xor.Left(failures) => throw failures.fillInStackTrace
    }
  }

  case class Field(name: String, schema: Root[_])

  case class Root[A : TypEv]
    (id: Option[String] = None, title: Option[String] = None, description: Option[String] = None,
    definition: Option[Root[_]] = None, properties: Option[List[Field]] = None,
    typ: Option[A] = None, enum: Option[List[String]] = None,
    oneOf: Option[SchemaArray] = None, anyOf: Option[SchemaArray] = None, allOf: Option[SchemaArray] = None,
    not: Option[Root[_]] = None,
    required: Option[StringArray] = None)

  type SchemaArray = List[Root[_]]
  type PositiveInteger = Int
  type StringArray = List[String]

  def polyDecode[A : Decoder : TypEv, B : Decoder : TypEv](c: ACursor) = {
    val res = c.as[Option[A]] orElse c.as[Option[B]]

    // YUK. Have to cast here to something that fools the type checker. So we cast to the intersection of A and B
    // doesn't matter at runtime because it becomes Any, but for now we need something that satifisies that TypeClass
    // constraint
    res.asInstanceOf[ Xor[DecodingFailure, Option[ (A Or B)#intersect ]] ]
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
        case "array" => Xor.right(SimpleTypes.Array)
        case "boolean" => Xor.right(SimpleTypes.Boolean)
        case "integer" => Xor.right(SimpleTypes.Integer)
        case "null" => Xor.right(SimpleTypes.Null)
        case "number" => Xor.right(SimpleTypes.Number)
        case "object" => Xor.right(SimpleTypes.Object)
        case "string" => Xor.right(SimpleTypes.String)
        case t@_ => Xor.left(DecodingFailure("Don't know simple type " + t, c.history))
      }
    } yield typ)

  implicit def SimpleTypeEncoder: Encoder[SimpleType] = Encoder.instance(_.name.asJson)

//  def cast(a: Any, tt: scala.Symbol) = tt match {
//    case 'SimpleType => a.asInstanceOf[Decoder.Result[Option[SimpleType]]]
//    case 'ListSimpleType => a.asInstanceOf[Decoder.Result[Option[List[SimpleType]]]]
//  }

  implicit val RootDecoder: Decoder[Root[_]] =
    Decoder.instance((c) => for {
      id <- c.downField("id").as[Option[String]]
      title <- c.downField("title").as[Option[String]]
      description <- c.downField("description").as[Option[String]]
      definition <- c.downField("definition").as[Option[Root[_]]]
      properties <- c.downField("properties").as[Option[List[Field]]]
      typ <- polyDecode[SimpleType, List[SimpleType]](c.downField("type"))
    } yield Root(id, title, description, definition, properties, typ))

  implicit val  RootEncoder: Encoder[Root[_]] =
    Encoder.instance((r: Root[_]) => Json.obj(
      "id" -> r.id.asJson,
      "title" -> r.title.asJson,
      "description" -> r.description.asJson,
      "definition" -> r.definition.asJson,
      "properties" -> r.definition.asJson
//      "typ" -> r.typ.asJson
    ))

    implicit val FieldDecoder: Decoder[List[Field]] =
      Decoder.instance((c) => {
        c.focus.asObject match {

          // Extract each field and parse it's sub-schema as a an Root schema
          case Some(obj) => {

            val results = for {
              (name, json) <- obj.toList
              result = json.as[Root[_]].map(Field(name, _))
            } yield result

            // sequence() is cats magic to make List(Xor.Right(1), Xor.Right(2)) into Xor(List(1,2)), used here ensure
            // that if parse errors occured within fields, those errors as propogated up
            // NB: explicit type is just to help IntelliJ realize this is valid. Not really required.
            results.sequenceU: Xor[DecodingFailure, List[Field]]
          }

          // Properties isn't an object?!?
          case _ => Xor.left(DecodingFailure("Properties isn't an object?", c.history))
        }

      })

    implicit val FieldEncoder: Encoder[List[Field]] =
      Encoder.instance((lf: List[Field]) => Json.Null)

}

