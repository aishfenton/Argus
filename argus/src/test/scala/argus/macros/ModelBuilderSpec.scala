package argus.macros

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Aish Fenton.
  */
class ModelBuilderSpec extends FlatSpec with Matchers with ASTMatchers {

  import runtimeUniverse._
  val mb = new ModelBuilder[runtimeUniverse.type](runtimeUniverse)
  import argus.schema.Schema._

  "mkIntriniscType()" should "create types for basic types" in {
    mb.mkIntrinsicType(SimpleTypes.String) should === (tq"String")
    mb.mkIntrinsicType(SimpleTypes.Boolean) should === (tq"Boolean")
    mb.mkIntrinsicType(SimpleTypes.Integer) should === (tq"Int")
    mb.mkIntrinsicType(SimpleTypes.Number) should === (tq"Double")
  }

  "mkCaseClassDef()" should "create a case class with an optional field for each simple type" in {
    val fields = Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("b", schemaFromSimpleType(SimpleTypes.String)) :: Nil

    val (typ, res) = mb.mkCaseClassDef(List("Foo"), "Bar", None, fields, None)

    typ should === (tq"Foo.Bar")
    res should === (q"case class Bar(a: Option[Int] = None, b: Option[String] = None)" :: Nil)
  }

  it should "make 'required' fields non optional" in {
    val fields =
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("b", schemaFromSimpleType(SimpleTypes.String)) :: Nil

    val (_, res) = mb.mkCaseClassDef(List("Foo"), "Bar", None, fields, Some("b" :: Nil))

    res should === (q"case class Bar(a: Option[Int] = None, b: String)" :: Nil)
  }

  it should "reference other classes when type is $ref" in {
    val fields = Field("a", schemaFromRef("#/definitions/Address")) :: Nil
    val (_, res) = mb.mkCaseClassDef(List("Foo"), "Bar", None, fields, None)

    res should === (q"case class Bar(a: Option[Address] = None)" :: Nil)
  }

  it should "create inner case classes when the schemas has sub-schemas" in {
    val innerSchema = schemaFromFields(
      Field("number", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("street", schemaFromSimpleType(SimpleTypes.String)) :: Nil
    )
    val fields =
      Field("name", schemaFromSimpleType(SimpleTypes.String)) ::
      Field("address", innerSchema) :: Nil

    val (_, res) = mb.mkCaseClassDef(List("Foo"), "Person", None, fields, None)
    res should === (
      q"case class Person(name: Option[String] = None, address: Option[Foo.Person.Address] = None)" ::
      q"""
      object Person {
        case class Address(number: Option[Int] = None, street: Option[String] = None)
      }
      """ :: Nil)
  }

  "mkEnumDef()" should "make a sum type (sealed trait + sub-classes) to represent enum types" in {
    val enums = "\"UK\"" :: "3.14" :: "1" :: Nil
    val (typ, res) = mb.mkEnumDef("Foo" :: Nil, "Bar", enums)

    typ should === (tq"Foo.Bar")
    res should === (
      q"@enum sealed trait Bar extends scala.Product with scala.Serializable { def json: String }" ::
      q"""
      object BarEnums {
        case object UK extends Bar { val json: String = "\"UK\"" }
        case object n314 extends Bar { val json: String = "3.14" }
        case object n1 extends Bar { val json: String = "1" }
      }
      """ :: Nil
    )
  }

  it should "annotate it with an @enum" in {
    val enums = "\"UK\"" :: "3.14" :: "1" :: Nil
    val (_, res) = mb.mkEnumDef(Nil, "Foo", enums)

    showCode(res.head) should include ("@enum")
  }

  it should "name enums with complex types something sensible (with the raw json included within)" in {
    val enums = """{ "albert" : "berty" }""" :: """{ "a" : [10, 3.14, 3.7e-5] }""" :: """{ "a" : { "b" : "c" } }""" :: Nil
    val (_, res) = mb.mkEnumDef(Nil, "Foo", enums)

    val q"object FooEnums { ..$defs }" = res(1)
    val names = defs map { case q"case object $name extends Foo { $_ }" => name } map(_.toString)

    names should === (List("AlbertBerty", "A1031437e5", "ABC"))
  }

  "mkUnionType()" should "make a type (sealed trait + sub-classes) to represent union types" in {
    val union = schemaFromSimpleType(SimpleTypes.Integer) :: schemaFromSimpleType(SimpleTypes.String) :: Nil
    val (typ, res) = mb.mkUnionTypeDef("Foo" :: Nil, "Bar", union)

    typ should === (tq"Foo.BarUnion")
    res should === (
      q"@union sealed trait BarUnion extends scala.Product with scala.Serializable" ::
      q"case class BarInt(x: Int) extends BarUnion" ::
      q"case class BarString(x: String) extends BarUnion" ::
      Nil
    )
  }

  it should "annotate it with an @union" in {
    val union = schemaFromSimpleType(SimpleTypes.Integer) :: schemaFromSimpleType(SimpleTypes.String) :: Nil
    val (_,res) = mb.mkUnionTypeDef(Nil, "Foo", union)

    showCode(res.head) should include ("@union")
  }

  it should "support $ref schemas within the union" in {
    val union =
      schemaFromRef("#/definitions/Address") ::
      schemaFromRef("#/definitions/Person") ::
      Nil

    val (_, res) = mb.mkUnionTypeDef(Nil, "Foo", union)

    res should === (
      q"@union sealed trait FooUnion extends scala.Product with scala.Serializable" ::
        q"case class FooAddress(x: Address) extends FooUnion" ::
        q"case class FooPerson(x: Person) extends FooUnion" ::
        Nil
    )
  }

  it should "support inline schemas within the union" in {
    val union =
      schemaFromFields(
        Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
        Field("b", schemaFromSimpleType(SimpleTypes.String)) ::
        Nil
      ) ::
      schemaFromFields(
        Field("c", schemaFromSimpleType(SimpleTypes.Number)) ::
        Field("d", schemaFromSimpleType(SimpleTypes.Number)) ::
        Nil
      ) ::
      Nil

    val (_, res) = mb.mkUnionTypeDef(Nil, "Foo", union)

    res should === (
      q"@union sealed trait FooUnion extends scala.Product with scala.Serializable" ::
        q"case class FooFoo1(x: Foo1) extends FooUnion" ::
        q"case class FooFoo2(x: Foo2) extends FooUnion" ::
        q"case class Foo1(a: Option[Int] = None, b: Option[String] = None)" ::
        q"case class Foo2(c: Option[Double] = None, d: Option[Double] = None)" ::
        Nil
    )
  }

  it should "support array schemas within the union" in {
    val union =
      schemaFromArray(schemaFromSimpleType(SimpleTypes.Integer)) ::
      schemaFromArray(schemaFromSimpleType(SimpleTypes.String)) ::
      Nil

    val (_, res) = mb.mkUnionTypeDef(Nil, "Foo", union)

    res should === (
      q"@union sealed trait FooUnion extends scala.Product with scala.Serializable" ::
      q"case class FooListInt(x: List[Int]) extends FooUnion" ::
      q"case class FooListString(x: List[String]) extends FooUnion" ::
      Nil
    )
  }

  "mkDef()" should "create a type alias for a $ref schema" in {
    val schema = schemaFromRef("#/definitions/ABC")
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)

    typ should === (tq"Foo.Bar")
    res should === (
      q"type Bar = ABC" ::
      Nil
    )
  }

  it should "create an enum for a enum schema" in {
    val schema = schemaFromEnum("\"A\"" :: "\"B\"" :: Nil)
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)

    typ should === (tq"Foo.Bar")
    showCode(res.head) should include ("@enum")
  }

  it should "create an case class for a object schema named using name" in {
    val schema = schemaFromFields(
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Nil
    )
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)

    typ should === (tq"Foo.Bar")
    showCode(res.head) should include ("case class Bar")
  }

  it should "create an type alias for an array schema named using name" in {
    val schema = schemaFromArray(schemaFromSimpleType(SimpleTypes.String))
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)

    typ should === (tq"Foo.Bar")
    showCode(res.head) should include ("type Bar = List[String]")
  }

  it should "create an type alias for intrinsic types" in {
    val schema = schemaFromSimpleType(SimpleTypes.String)
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)

    typ should === (tq"Foo.Bar")
    showCode(res.head) should include ("type Bar = String")
  }

  it should "create union types for oneOfs" in {
    val schema = schemaFromUnionType(
      schemaFromSimpleType(SimpleTypes.Integer) ::
      schemaFromArray(schemaFromSimpleType(SimpleTypes.String)) ::
      Nil
    )
    val (typ, res) = mb.mkDef("Foo" :: Nil, "Bar", None, schema)
    val code = res.map(showCode(_)).mkString("\n")

    typ should === (tq"Foo.BarUnion")
    code should include ("@union sealed trait BarUnion")
    code should include ("case class BarInt")
    code should include ("case class BarListString")
  }

  "myTyp()" should "make a type from a $ref" in {
    val schema = schemaFromRef("#/definitions/ABC")
    val (typ, defs) = mb.mkType(Nil, schema, "Foo")

    typ should === (tq"ABC")
    defs should be (empty)
  }

  it should "make a typ from an array" in {
    val schema = schemaFromArray(schemaFromSimpleType(SimpleTypes.Integer))
    val (typ, defs) = mb.mkType(Nil, schema, "Foo")

    typ should === (tq"List[Int]")
    defs should be (empty)
  }

  it should "make a typ from an array with an inline object definition" in {
    val schema = schemaFromArray(schemaFromFields(
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("b", schemaFromRef("#/definitions/B")) ::
      Nil
    ))
    val (typ, defs) = mb.mkType(Nil, schema, "Foo")

    typ should === (tq"List[Foo]")
    defs.map(showCode(_)).mkString("\n") should include("case class Foo(a: Option[Int] = None, b: Option[B] = None)")
  }

  it should "make a typ from intrinsic types" in {
    mb.mkType(Nil, schemaFromSimpleType(SimpleTypes.Integer), "Foo")._1 should === (tq"Int")
    mb.mkType(Nil, schemaFromSimpleType(SimpleTypes.Number), "Foo")._1 should === (tq"Double")
    mb.mkType(Nil, schemaFromSimpleType(SimpleTypes.String), "Foo")._1 should === (tq"String")
    mb.mkType(Nil, schemaFromSimpleType(SimpleTypes.Boolean), "Foo")._1 should === (tq"Boolean")
  }

  it should "make a type from any inline definitions (with the appropriate path)" in {
    val schema = schemaFromFields(
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("b", schemaFromSimpleType(SimpleTypes.Number)) ::
      Nil
    )
    val (typ, defs) = mb.mkType(List("Root"), schema, "Foo")

    typ should === (tq"Root.Foo")
    showCode(defs.head) should include ("case class Foo")
  }

  it should "default to making a wrapper class around an Any field" in {
    val (typ, defs) = mb.mkType("Foo" :: Nil, Root(), "Bar")

    typ should === (tq"Foo.Bar")
    defs should === (q"case class Bar(x: Any)" :: Nil)
  }

  "mkValDef()" should "make a valDefs from a Field" in {
    val (valDef, defDefs) = mb.mkValDef(Nil, Field("a", schemaFromSimpleType(SimpleTypes.Integer)), true)

    valDef should === (q"val a: Option[Int] = None")
    defDefs should be (empty)
  }

  it should "make required types non optional" in {
    val (valDef, defDefs) = mb.mkValDef(Nil, Field("a", schemaFromSimpleType(SimpleTypes.Number)), false)

    valDef should === (q"val a: Double")
    defDefs should be (empty)
  }

  it should "make a type named from an inline schema definition named after the parameter name (with the appropriate path)" in {
    val schema = schemaFromFields(
      Field("b", schemaFromFields(
        Field("c", schemaFromSimpleType(SimpleTypes.String)) ::
        Nil
      )) ::
      Nil
    )
    val (valDef, defDefs) = mb.mkValDef(List("Root", "Foo"), Field("a", schema), true)
    val defCode = defDefs.map(showCode(_)).mkString

    valDef should === (q"val a: Option[Root.Foo.A] = None")
    defCode should include ("case class A(b: Option[Root.Foo.A.B] = None)")
    defCode should include ("case class B(c: Option[String] = None")
  }

  "mkTypeAlias()" should "make a type alias" in {
    val (typ, res) = mb.mkTypeAlias("Foo" :: Nil, "Bar", tq"xyzzy.Int")
    typ should === (tq"Foo.Bar")
    res should === (List(q"type Bar = xyzzy.Int"))
  }

  "mkSchemaDef()" should "make a schema" in {
    val base = schemaFromFields(
      Field("name",    schemaFromRef("#/definitions/Name")) ::
      Field("address", schemaFromRef("#/definitions/Address")) ::
      Field("id",      schemaFromSimpleType(SimpleTypes.Integer)) ::
      Nil
    )
    val defs =
      Field("Name", schemaFromSimpleType(SimpleTypes.String)) ::
      Field("Address", schemaFromFields(
        Field("number", schemaFromSimpleType(SimpleTypes.Integer)) :: Nil
      )) ::
      Nil

    val (typ: Tree, res) = mb.mkSchemaDef("Root", None, schema=base.copy(definitions=Some(defs)), "Foo" :: Nil)
    val code = res.map(showCode(_)).mkString("\n")

    typ should === (tq"Foo.Root")
    code should include ("case class Root(name: Option[Name] = None, address: Option[Address] = None, id: Option[Int] = None)")
    code should include ("type Name = String")
    code should include ("case class Address(number: Option[Int] = None)")
  }

  it should "support definitions nested within properties" in {
    val innerDef =
      Field("C", schemaFromSimpleType(SimpleTypes.String)) ::
      Nil

    val schema = schemaFromFields(
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
      Field("b", schemaFromRef("#/properties/b/definitions/C").copy(definitions=Some(innerDef))) ::
      Nil
    )

    val (typ: Tree, res) = mb.mkSchemaDef("Root", None, schema)
    val code = res.map(showCode(_)).mkString("\n")
    code should include ("case class Root(a: Option[Int] = None, b: Option[Root.C] = None)")
    code should include ("type C = String")
  }

  it should "support extending a provided parent" in {
    val schema = schemaFromFields(
      Field("a", schemaFromSimpleType(SimpleTypes.Integer)) ::
        Nil
    )

    val (typ: Tree, res) = mb.mkSchemaDef("Root", Some("ParentTrait"), schema)
    val code = res.map(showCode(_)).mkString("\n")
    code should include ("case class Root(a: Option[Int] = None) extends ParentTrait")
  }

}
