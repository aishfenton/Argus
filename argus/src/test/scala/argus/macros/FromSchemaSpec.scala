package argus.macros

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Aish Fenton.
  */
class FromSchemaSpec extends FlatSpec with Matchers {

  @fromSchemaResource("/simple.json")
  object Test

  "FromSchema macro" should "create a root case class containing a field for each property" in {
    import Test._

    val root = new Root(Some("Bob"), Some("Jones"), Some(4))
    root.firstName should === (Some("Bob"))
    root.lastName should === (Some("Jones"))
    root.age should === (Some(4))
  }

  it should "create a case class for every definition that is itself an object" in {
    import Test._

    val address = Address(number=Some(40), street=Some("Main St"))
    val root = new Root(address = Some(address))

    root.address.get.number should === (Some(40))
    root.address.get.street should === (Some("Main St"))
  }

  it should "create a type alias for every definition that is an intrinsic type" in {
    import Test._

    implicitly[ErdosNumber =:= Int]
    val root = new Root(erdosNumber = Some(1))
    root.erdosNumber should === (Some(1))
  }

  it should "produce types for nested properties (and name them after the field name)" in {
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

    val a = A(Some("bar"))
    Root(Some(a)).a.get.b.get should === ("bar")
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

  it should "make enum types into sum types" in {
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

    val root = Root(Some(CountryEnum.NZ))
    root.country should === (Some(CountryEnum.NZ))
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

    WeirdoEnum.Bob.json should === (""""Bob"""")
    WeirdoEnum.n4.json should === ("4")
    WeirdoEnum.AB.json should === ("""{"a":"b"}""")
  }

  it should "produce sum types for [one/any/all]Of schemas" in {
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

    WeirdoEnum.Bob.json should === (""""Bob"""")
    WeirdoEnum.n4.json should === ("4")
    WeirdoEnum.AB.json should === ("""{"a":"b"}""")
  }

  it should "compile when we use a complex real-life schema" in {
//    @fromSchemaResource("/vega-lite-schema.json")
//    object Vega
  }

}
