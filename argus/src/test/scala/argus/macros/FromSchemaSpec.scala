package argus.macros

import java.io.File

import argus.json.JsonDiff
import argus.schema.Schema
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax.either._
import io.circe._
import io.circe.syntax._

import scala.io.Source

class FromSchemaSpec extends FlatSpec with Matchers with JsonMatchers {

  "Making schemas" should "build case classes" in {
    @fromSchemaJson("""
      {
        "definitions": {
          "Address": {
            "type": "object",
            "properties": {
              "number": { "type": "integer" },
              "street": { "type": "string" }
            }
          },
          "SSN": { "type": "string" }
        },
        "type": "object",
        "properties": {
          "name": { "type": "string" },
          "address": { "$ref": "#/definitions/Address" },
          "ssn": { "$ref": "#/definitions/SSN" }
        },
        "required" : ["name"]
      }
    """)
    object Foo
    import Foo._

    val address = new Address(number=Some(101), street=Some("Main St"))
    val root = new Root("Fred", Some(address), Some("107-245"))
    root.name should === ("Fred")
    root.address should === (Some(Address(Some(101), Some("Main St"))))
    root.ssn should === (Some("107-245"))
  }

  it should "build nested schemas (and name them after the field name)" in {
    @fromSchemaJson("""
      {
        "type": "object",
        "properties": {
          "a": {
            "type": "object",
            "properties": {
              "b" : { "type": "string" }
            }
          }
        }
      }
    """)
    object Foo
    import Foo._

    val a = Root.A(Some("bar"))
    Root(Some(a)).a.get.b.get should === ("bar")
  }

  it should "build enum types" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """)
    object Foo
    import Foo._

    val root = Root(Some(Root.CountryEnums.NZ))
    root.country should === (Some(Root.CountryEnums.NZ))
  }

  it should "build union types" in {
    @fromSchemaJson("""
    {
      "type" : "object",
      "definitions": {
        "Street": {
          "type": "object",
          "properties" : { "street" : { "type" : "string" } },
          "required" : ["street"]
        }
      },
      "properties": {
        "address": {
          "oneOf": [
            { "type": "string" },
            { "$ref": "#/definitions/Street" }
          ]
        }
      }
    }
    """)
    object Foo
    import Foo._

    val string = Root.AddressString("Main St")
    val street = Root.AddressStreet(Street("1010 Main St"))
    Root(Some(street)).address.get match {
      case Root.AddressStreet(st: Street) => st === (Street("1010 Main St"));
      case _ => fail("Didn't match type")
    }
  }

  it should "build array types" in {
    @fromSchemaJson("""
    {
      "definitions" : {
        "Editor" : {
          "type" : "object",
          "properties" : {
            "name" : { "type" : "string" },
            "location" : { "type" : "string" }
          }
        }
      },
      "type" : "object",
      "properties": {
        "days" : { "type" : "array", "items": { "type" : "string" } },
        "people": {
          "type": "array",
          "items": { "$ref": "#/definitions/Editor" }
        }
      }
    }
    """)
    object Foo
    import Foo._

    val ed1 = Editor(Some("Bob"), Some("CA"))
    val ed2 = Editor(Some("Jill"), Some("NY"))
    val root = Root(people=Some(List(ed1, ed2)), days=Some(List("Mon", "Fri")))
    root.people should === (Some(List(ed1, ed2)))
    root.days should === (Some(List("Mon", "Fri")))
  }

  it should "build type alias's for simple definitions" in {
    @fromSchemaJson("""
    {
      "definitions" : {
        "SSN" : { "type": "string" },
        "Names" : { "type": "array", "items": { "type": "string" } }
      }
    }
    """)
    object Foo
    import Foo._

    implicitly[SSN =:= String]
    implicitly[Names =:= List[String]]
  }

  "Building Circe Codecs" should "encode case classes" in {
    @fromSchemaResource("/simple.json", jsonEng=Some(JsonEngs.Circe))
    object Foo
    import Foo._
    import Foo.Implicits._

    val address = Address(Some(31), Some("Main St"))
    val root = Root(Some(List("Bob", "Smith")), Some(26), Some(address), Some(123))

    root.asJson should beSameJsonAs ("""
      |{
      |  "name" : [ "Bob", "Smith" ],
      |  "age" : 26,
      |  "address" : {
      |    "number" : 31,
      |    "street" : "Main St"
      |  },
      |  "erdosNumber": 123
      |}
    """.stripMargin)
  }

  it should "decode case classes" in {
    @fromSchemaResource("/simple.json")
    object Foo
    import Foo._
    import Foo.Implicits._

    val json = """
      |{
      |  "name" : [ "Bob", "Smith" ],
      |  "age" : 26,
      |  "address" : {
      |    "number" : 31,
      |    "street" : "Main St"
      |  },
      |  "erdosNumber": 123
      |}
    """.stripMargin
    val root = parser.decode[Root](json).toOption.get

    root.name should === (Some(List("Bob", "Smith")))
    root.age should === (Some(26))
    root.address should === (Some(Address(Some(31), Some("Main St"))))
    root.erdosNumber === (Some(123))
  }

  it should "encode enum types" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._
    import io.circe.syntax._

    val root = Root(Some(Root.CountryEnums.NZ))
    root.asJson should beSameJsonAs ("""
      |{
      |  "country": "NZ"
      |}
    """.stripMargin)
  }

  it should "decode enum type" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._

    val json =
      """
        |{
        |  "country": "NZ"
        |}
      """.stripMargin
    val root = parser.decode[Root](json).toOption.get

    root.country should === (Some(Root.CountryEnums.NZ))
  }

  it should "return a DecodeFailure if it can't decode an enum type" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._

    val json =
      """
        |{
        |  "country": "oops"
        |}
      """.stripMargin
    parser.decode[Root](json).isLeft shouldBe (true)
  }

  it should "encode any wrappers" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "misc": { }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._

    val values = Map("a" -> 1, "b" -> List(1.1, 2.2), "c" -> Map( "d" -> "bar", "e" -> 3.14 ))
    val root = Root(Some(Root.Misc(values)))

    root.asJson should beSameJsonAs("""
    {
      "misc": { "a": 1, "b": [1.1, 2.2], "c": { "d": "bar", "e": 3.14 } }
    }
    """)
  }

  it should "encode any wrappers with array types (which need special handling)" in {
      @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "misc": { }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._

    val values = Array(Array("a", 1), Array(1), Array(2.2), Array(3L), Array(true), Array(3.toShort),
                       Array(3.0f), Array("a"), Array(2, 2.2))
    val root = Root(Some(Root.Misc(values)))

    root.asJson should beSameJsonAs("""
    {
      "misc": [ ["a", 1], [1], [2.2], [3], [true], [3], [3.0], ["a"], [2, 2.2] ]
    }
    """)
  }

  it should "decode any wrappers" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "misc": { }
      }
    }
    """)
    object Foo
    import Foo._
    import Foo.Implicits._

    val json =
      """
        |{
        |  "misc": { "a": 1, "b": [1, 2.0, "foo"], "c": { "d": "bar" } }
        |}
      """.stripMargin

    val root = parser.decode[Root](json).toOption.get
    val values = Map("a" -> 1, "b" -> List(1, 2.0, "foo"), "c" -> Map( "d" -> "bar" ))
    root.misc should === (Some(Root.Misc(values)))
  }

  it should "let you override encoders/decoders with higher priority implicits" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "name": { "type" : "string" }
      }
    }
    """)
    object Foo
    import Foo._

    object Implicits extends Foo.LowPriorityImplicits {
      implicit val betterEncoder: Encoder[Foo.Root] = Encoder.instance { r => "override".asJson }
      implicit val betterDecoder: Decoder[Foo.Root] = Decoder.instance((h: HCursor) =>
        Either.right(Root(name=Some("override")))
      )
    }
    import Implicits._

    val root = parser.decode[Root]("""{ "name": "fred" }""").toOption.get
    root.name should === (Some("override"))
    root.copy(name=Some("james")).asJson should beSameJsonAs("\"override\"")
  }

  "Params" should "support outPath and write out the generated code" in {
    @fromSchemaResource("/simple.json", outPath=Some("/tmp/Simple.scala"))
    object Simple

    val file = new File("/tmp/Simple.scala")
    file should exist

    val lines = Source.fromFile(file).getLines.toList
    lines.head should === ("object Simple {")
    lines.size should be >= 10
  }

  it should "support name, and name the root element using it" in {
    @fromSchemaResource("/simple.json", name="Person")
    object Schema

    Schema.Person(age=Some(42)).age should === (Some(42))
  }

  it should "support parent, and make the root element extend it" in {
    trait Person
    @fromSchemaResource("/simple.json", parent=Some("Person"))
    object Schema

    implicitly[Schema.Root <:< Person]
  }

  "Complex example" should "work end to end" in {
    @fromSchemaResource("/vega-lite-schema.json")
    object Vega
    import Vega._
    import Vega.Implicits._
    import io.circe.syntax._

    val json =
      """
        |{
        |  "description": "A bar chart showing the US population distribution of age groups and gender in 2000.",
        |  "data": { "values": [ {"a": 1, "b" : 2.0, "c": "NZ" }, {"a": 2, "b": 3.14, "c": "US" } ] },
        |  "transform": {
        |    "filter": "datum.year == 2000",
        |    "calculate": [{"field": "gender", "expr": "datum.sex == 2 ? \"Female\" : \"Male\""}]
        |  },
        |  "mark": "bar",
        |  "encoding": {
        |    "x": {
        |      "field": "age", "type": "ordinal",
        |      "scale": {"bandSize": 17}
        |    },
        |    "y": {
        |      "aggregate": "sum", "field": "people", "type": "quantitative",
        |      "axis": {"title": "population"}
        |    },
        |    "color": {
        |      "field": "gender", "type": "nominal",
        |      "scale": {"range": ["#e377c2","#1f77b4"]}
        |    }
        |  },
        |  "config": {
        |    "mark": {"opacity": 0.6, "stacked" : "none"}
        |  }
        |}
      """.stripMargin

    val res = parser.decode[RootUnion](json).toOption.get
    res.asJson should beSameJsonAs(json)
  }

}
