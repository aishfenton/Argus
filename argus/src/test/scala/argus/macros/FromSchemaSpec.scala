package argus.macros

import argus.schema.Schema
import org.scalatest.{FlatSpec, Matchers}
import io.circe._
import io.circe.syntax._

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

  "Building Circe Codecs" should "have an encoder for each case class" in {
    @fromSchemaResource("/simple.json", jsonEng=Some(JsonEngs.Circe))
    object Foo
    import Foo._
    import Foo.Implicits._

    val address = Address(Some(31), Some("Main St"))
    val root = Root(Some(List("Bob", "Smith")), Some(26), Some(address), Some(123))

    root.asJson should noDifferentFrom ("""
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

  it should "have an encoder for each enum type" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """, jsonEng=Some(JsonEngs.Circe))
    object Foo
    import Foo._
    import Foo.Implicits._
    import io.circe.syntax._

    val root = Root(Some(Root.CountryEnums.NZ))
    root.asJson should noDifferentFrom ("""
      |{
      |  "country": "NZ"
      |}
    """.stripMargin)
  }

  "Building Circe Codecs" should "have a decoder for each case class" in {
    @fromSchemaResource("/simple.json", jsonEng=Some(JsonEngs.Circe))
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

  it should "have a decoder for each enum type" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "country": { "enum": ["UK", "USA", "NZ"] }
      }
    }
    """, jsonEng=Some(JsonEngs.Circe))
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

  "Complex schema" should "work end to end" in {
//    @fromSchemaResource("/vega-lite-schema.json", outPath=Some("/tmp/Vega.scala"))
//    object Vega
//    import Vega._
//    import Vega.Implicits._
//    import io.circe.syntax._

    val json =
      """
        |{
        |  "description": "A bar chart showing the US population distribution of age groups and gender in 2000.",
        |  "data": { "url": "data/population.json"},
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

//    val res = parser.decode[Root](json).toOption.get
//    res.asJson should noDifferentFrom(json)
  }

}
