package argus.macros

import org.scalactic.Equality
import org.scalatest._

import scala.language.experimental.macros

/**
  * @author Aish Fenton.
  */
class ASTHelpersSpec extends FlatSpec with Matchers with ASTMatchers {

  import runtimeUniverse._

  val helpers = new ASTHelpers[runtimeUniverse.type](runtimeUniverse)
  import helpers._

  "hasAnnotation" should "return true if the given mods has the given annotation" in {
    val q"$mods trait $_" = q"@foo trait Test"
    hasAnnotation(mods, "foo") should be (true)
  }

  "selectPathToList()" should "make a.b.Bob into List[String](a,b,Bob)" in {
    selectPathToList(q"a.b.Bob") should ===(List("a", "b", "Bob"))
  }

  "typeSelectPathToList()" should "make type a.b.Bob into List[String](a,b,Bob)" in {
    typeSelectPathToList(tq"a.Bob") should ===(List("a", "Bob"))
    typeSelectPathToList(tq"a.b.Bob") should ===(List("a", "b", "Bob"))
  }

  it should "make type a.b.Bob[A,B[C]] into List[String](a,b,BobABC)" in {
    typeSelectPathToList(tq"a.b.Bob[A,B[C]]") should ===(List("a", "b", "BobABC"))
  }

  "nameFromType()" should "make type into a string that can be used as a name" in {
    nameFromType(tq"a.b.c.MyObj") should === ("ABCMyObj")
  }

  it should "let you specify not to use the path" in {
    nameFromType(tq"a.b.MyObj", false) should === ("MyObj")
  }

  it should "work with type parameters" in {
    nameFromType(tq"a.b.MyObj[A, B[C]]") should === ("ABMyObjABC")
  }

  "inOption()" should "wrap a given type in Option[X]" in {
    val i = tq"Int"
    inOption(i) should ===(tq"Option[Int]")
  }

  "inList()" should "wrap a given type in List[X]" in {
    val i = tq"Int"
    inList(i) should ===(tq"List[Int]")
  }

  "mkSelectPath()" should "build a tree that represents a path of objects (e.g. obj1.obj2.myObj)" in {
    mkSelectPath(List("obj1", "obj2", "myObj")) should ===(q"obj1.obj2.myObj")
  }

  "mkTypeSelectPath()" should "build a tree that represents a Type reference via a select path (e.g. obj1.obj2.MyObj)" in {
    mkTypeSelectPath(List("obj1", "obj2", "MyObj")) should ===(tq"obj1.obj2.MyObj")
  }

  "extractPathFromRef()" should "extract a path to the type reference from a given json-schema $ref" in {
    val ref = "#/definitions/Person"
    extractPathFromRef(Some("Root"), ref) should === (List("Person"))
  }

  it should "work with nested definitions block" in {
    val ref = "#/properties/a/properties/b/properties/c/definitions/Person"
    extractPathFromRef(Some("Root"), ref) should === (List("Root","A","B","Person"))
  }

  "jsonToClassName" should "attempt to produce a type name from simple Json chunks" in {
    nameFromJson("\"one\"") should === ("One")
    nameFromJson("\"one-two\"") should === ("OneTwo")
    nameFromJson("\"one_two\"") should === ("One_two")
    nameFromJson("\"one two\"") should === ("OneTwo")
    nameFromJson("1") should === ("n1")
    nameFromJson("3.14") should === ("n314")
    nameFromJson("-3.14") should === ("n314")
  }

  "jsonToClassName" should "attempt to produce a type name from complex Json chunks" in {
    nameFromJson("""{ "a": "b" }""") should === ("AB")
    nameFromJson("""{ "num": 3.14  }""") should === ("Num314")
    nameFromJson("""{ "person": { "name": "Bob", "street": 34 }  }""") should === ("PersonNameBobStreet34")
  }

  "typeNameToTermName()" should "convert a typeName (with path) to a termName" in {
    companionForType(tq"a.b.C") should === (q"a.b.C")
  }

  "termNameToTypeName()" should "convert a termName (with path) to a typeName" in {
    typeForCompanion(q"a.b.C") should === (tq"a.b.C")
  }

  "extendsType()" should "return only case-case instances that extend the given type" in {
    val defs =
      q"case class A(i: Int)" ::
      q"case class B(i: Int) extends Bar" ::
      q"object Person { case class C(i: Int) extends Bar; case class D(i: Int) extends Foo }" ::
      q"case object E extends Bar { val i: Int = 1 }" ::
      Nil

    val res = collectExtendsType(List("Foo"), tq"Bar", defs)
    res.map(_._2) should === (
      q"case class B(i: Int) extends Bar" ::
      q"case class C(i: Int) extends Bar" ::
      q"case object E extends Bar { val i: Int = 1 }" ::
      Nil
    )
    res.map(_._1) should === (
      List("Foo") ::
      List("Foo", "Person") ::
      List("Foo") ::
      Nil
    )

  }

  "treesIntersect()" should "find the intersection of two lists of trees" in {
    treesIntersect(List(tq"Int", tq"foo.Bar"), List(tq"foo.Bar", q"foo.Bar")) should === (List(tq"foo.Bar"))
    treesIntersect(List(tq"Int", tq"foo.Bar"), List(q"foo.Bar")) should be (empty)
  }

  "paramsToMap" should "extract constant (or option(constant)) arguments  passed to a function" in {
    val q"myFunc(..$params)" = q""" myFunc(1, Some("foo"), c=2.0) """
    val res = paramsToMap(
      ("a", 3) :: ("b", None) :: ("c", 3.0) :: Nil,
      params
    )

    res should === (Map("a" -> 1, "b" -> Some("foo"), "c" -> 2.0))
  }

  it should "support named arguments" in {
    val q"myFunc(..$params)" = q""" myFunc(1, d=None, c=2.0) """
    val res = paramsToMap(
      ("a", 3) :: ("b", None) :: ("c", 3.0) :: ("d", Some(1)) :: Nil,
      params
    )

    res should === (Map("a" -> 1, "b" -> None, "c" -> 2.0, "d" -> None))
  }

  it should "support default values" in {
    val q"myFunc(..$params)" = q""" myFunc(b=Some(4)) """
    val res = paramsToMap(
      ("a", 3) :: ("b", None) :: ("c", Some("foo")) :: Nil,
      params
    )

    res should === (Map("a" -> 3, "b" -> Some(4), "c" -> Some("foo")))
  }

  it should "return the AST chunk when not sure what to do" in {
    val q"myFunc(..$params)" = q""" myFunc(Foo.Bar, Some(A.B)) """
    val res = paramsToMap(
      ("a", q"Foo.Baz") :: ("b", q"") :: Nil,
      params
    )

    res.map { case(k,v) => (k,showCode(v.asInstanceOf[Tree])) } should === (Map("a" -> "Foo.Bar", "b" -> "Some(A.B)"))
  }

}
