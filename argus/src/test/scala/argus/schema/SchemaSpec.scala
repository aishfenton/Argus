package argus.schema

import argus.json.JsonDiff
import cats.syntax.either._
import io.circe._
import org.scalatest._

import scala.io.Source

class SchemaSpec extends FlatSpec with Matchers {
  import Schema._

  def diffs(parsed: Root, original: String) = JsonDiff.diff(parsed.toJson, parser.parse(original).toOption.get)

  "Schema" should "round-trip a simple Json schema" in {
    val jsonStr = Source.fromInputStream(getClass.getResourceAsStream("/simple.json")).getLines.mkString("\n")
    val schema = Schema.fromResource("/simple.json")

    schema shouldBe a [Root]
    diffs(schema, jsonStr) shouldBe empty
  }

  it should "decode enum's into a list of Json entries" in {
    val json =
      """
        |{
        |  "type" : "object",
        |  "properties": {
        |    "country": {
        |      "enum" : ["USA", 4, { "a" : "b" }]
        |     }
        |  }
        |}
      """.stripMargin
    val schema = Schema.fromJson(json)

    val countryEnum = for {
      props <- schema.properties
      country <- props.find(_.name == "country")
      enum <- country.schema.enum
    } yield enum

    countryEnum should === (Some(List( "\"USA\"", "4", """{"a":"b"}""" )))
  }

  it should "round trip more complex schemas" in {
//    val jsonStr = Source.fromInputStream(getClass.getResourceAsStream("/meta-schema.json")).getLines.mkString("\n")
//    val schema = Schema.fromJson(jsonStr)
//
//    schema shouldBe a [Schema.Root]
//    diffs(schema, jsonStr) shouldBe empty

    pending
  }

}
