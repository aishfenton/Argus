package argus.macros

import argus.schema.Schema
import org.scalatest.{FlatSpec, Matchers}
import io.circe._
import io.circe.syntax._

/**
  * @author Aish Fenton.
  */
class FromSchemaSpec extends FlatSpec with Matchers {

  "Building objects" should "create a case class for every property that is an object" in {
    @fromSchemaJson("""
      {
        "type": "object",
        "properties": {
          "a": { "type": "integer" },
          "b": { "type": "number" },
          "socialSecurityNumber": { "type": "string" }
        }
      }
    """)
    object Foo
    import Foo._

    val root = new Root(Some(1), Some(3.14), Some("a"))
    root.a should === (Some(1))
    root.b should === (Some(3.14))
    root.socialSecurityNumber should === (Some("a"))
  }

  it should "wrap every field in an Option except those that are 'required'" in {
    @fromSchemaJson("""
      {
        "type": "object",
        "properties": {
          "a": { "type": "integer" },
          "b": { "type": "integer" },
          "c": { "type": "string" },
          "d": { "type": "string" }
        },
        "required": ["c", "a"]
      }
    """)
    object Foo
    import Foo._

    val root = Root(1, Some(2), "c", Some("d"))
    root.a should === (1)
    root.c should === ("c")
  }

  it should "create classes for nested sub-objects (and name them after the field name)" in {
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

  "Building definitions" should "create a case class for every object definition" in {
    @fromSchemaJson("""
      {
        "definitions" : {
          "Address" : {
            "type": "object",
            "properties" : {
              "number" : { "type" : "integer" },
              "street" : { "type" : "string" }
            }
          },
          "Name" : {
            "type": "object",
            "properties" : { "first" : { "type" : "string" } }
          }
        }
      }
    """)
    object Foo
    import Foo._

    Address(number=Some(40), street=Some("Main St"))
    Name(Some("Bob"))
  }

  it should "create a type alias for every definition that is an intrinsic type" in {
    @fromSchemaJson("""
      {
        "definitions" : {
          "ErdosNumber" : { "type": "integer" },
          "Name" : { "type" : "string" }
        }
      }
    """)
    object Foo
    import Foo._

    implicitly[ErdosNumber =:= Int]
    implicitly[Name =:= String]
  }

  it should "resolve references to definitions through $ref" in {
    @fromSchemaJson("""
      {
        "definitions" : {
          "Address" : {
            "type": "object",
            "properties" : { "street" : { "type" : "string" } }
          },
          "Name" : { "type": "string" }
        },
        "type" : "object",
        "properties" : {
          "address" : { "$ref" : "#/definitions/Address" },
          "name" : { "$ref" : "#/definitions/Name" }
        }
      }
    """)
    object Foo
    import Foo._

    val address = Address(Some("Main St"))
    Root(address=Some(address), name=Some("Bob")).address.get.street === (Some("Main St"))
  }

  it should "support definitions nested within properties" in {
    @fromSchemaJson("""
      {
        "type": "object",
        "properties": {
          "a": {
            "definitions": {
              "Person": { "type": "object", "properties": { "name": { "type": "string" } } }
            },
            "type": "object",
            "properties": {
              "b" : { "$ref": "#/properties/a/definitions/Person" }
            }
          }
        }
      }
    """)
    object Foo
    import Foo._

    val a = Root.A(Some(Root.Person(Some("bob"))))
    Root(Some(a)).a.get.b.get should === (Root.Person(Some("bob")))
  }

  "Enum Types" should "make enum types into sum types" in {
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

    val root = Root(Some(Root.CountryEnum.NZ))
    root.country should === (Some(Root.CountryEnum.NZ))
  }

  it should "name enums with complex types something sensible, but have rawJson encoded within it" in {
    @fromSchemaJson("""
    {
      "type": "object",
      "properties": {
        "weirdo": { "enum": ["Bob", 4, { "a" : "b" }] }
      }
    }
    """)
    object Foo
    import Foo._

    Root.WeirdoEnum.Bob.json should === (""""Bob"""")
    Root.WeirdoEnum.n4.json should === ("4")
    Root.WeirdoEnum.AB.json should === ("""{"a":"b"}""")
  }

  "oneOf" should "produce a sum types covering the type options" in {
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
    Root(Some(street)).address.get match { case Root.AddressStreet(st: Street) => st === (Street("1010 Main St")) }
  }

  it should "support inline schemas" in {
    @fromSchemaJson("""
    {
      "oneOf" : [
        { "type": "integer" },
        { "type": "object", "properties" : { "a" : { "type" : "string" } } }
      ]
    }
    """)
    object Foo
    import Foo._

    val a = RootInt(4)
    val b = RootRoot2(Root2(a=Some("b")))
  }

  "[all/any]Of" should "produce a List of the sum type covering it's types" in {
    pending
  }

  "Building arrays" should "support array types of intrinsic types" in {
    @fromSchemaJson("""
    {
      "type" : "object",
      "properties": {
        "people": {
          "type": "array",
          "items": { "type": "string" }
        }
      }
    }
    """)
    object Foo
    import Foo._

    Root(Some(List("Bob", "Alice"))).people should === (Some(List("Bob", "Alice")))
  }

  it should "support array types of references" in {
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
    Root(Some(List(ed1, ed2))).people should === (Some(List(ed1, ed2)))
  }

  it should "support array types when in definitions" in {
    @fromSchemaJson("""
    {
      "definitions" : {
        "People": {
          "type": "array",
          "items": {
            "type" : "object",
            "properties" : {
              "name" : { "type" : "string" }
            }
          }
        },
        "Locations": {
          "type": "array",
          "items": { "type" : "string" }
        }
      }
    }
    """)
    object Foo
    import Foo._

    implicitly[People =:= List[PeopleItem]]
    implicitly[Locations =:= List[String]]
  }

  it should "support array types with inline schemas" in {
    @fromSchemaJson("""
    {
      "type" : "object",
      "properties": {
        "people": {
          "type": "array",
          "items": {
            "type" : "object",
            "properties" : {
              "name" : { "type" : "string" }
            }
          }
        }
      }
    }
    """)
    object Foo
    import Foo._

    val p1 = Root.People(Some("Bob"))
    val p2 = Root.People(Some("Jill"))
    Root(Some(List(p1, p2))).people should === (Some(List(p1, p2)))
  }

  "Building Circe Codecs" should "add encoders for each case class" in {
    @fromSchemaJson("""
    {
      "definitions" : {
        "Address" : {
          "type" : "object",
          "properties" : {
            "number" : { "type" : "integer" },
            "street" : { "type" : "string" }
          }
        }
      },
      "type" : "object",
      "properties": {
        "name": { "type": "string" },
        "address": { "$ref": "#/definitions/Address" }
      }
    }
    """, Some(JsonEngs.Circe))
    object Foo
    import Foo._
    import Foo.Implicits._

    val address = Address(Some(31), Some("Main St"))
    val root = Root(Some("Bob"), Some(address))
    println( root.asJson )
  }

  "Complex schema" should "work end to end" in {
//    @fromSchemaResource("/vega-lite-schema.json")
//    object Vega
  }

}
