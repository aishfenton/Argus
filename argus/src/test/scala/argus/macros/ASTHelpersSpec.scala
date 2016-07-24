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

  "typeToTermName()" should "make type into a TermName with initial lower case letter" in {
    typeToTermName(TypeName("MyObj")) should === (TermName("myObj"))
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
    extractPathFromRef(List("Foo", "Root"), ref) should === (List("Foo","Person"))
  }

  it should "work with nested definitions block" in {
    val ref = "#/properties/a/properties/b/properties/c/definitions/Person"
    extractPathFromRef(List("Foo", "Root"), ref) should === (List("Foo","Root","A","B","Person"))
  }

  "jsonToClassName" should "attempt to produce a type name from simple Json chunks" in {
    typeNameFromJson("\"one\"") should === ("One")
    typeNameFromJson("\"one-two\"") should === ("OneTwo")
    typeNameFromJson("\"one_two\"") should === ("One_two")
    typeNameFromJson("\"one two\"") should === ("OneTwo")
    typeNameFromJson("1") should === ("n1")
    typeNameFromJson("3.14") should === ("n314")
    typeNameFromJson("-3.14") should === ("n314")
  }

  "jsonToClassName" should "attempt to produce a type name from complex Json chunks" in {
    typeNameFromJson("""{ "a": "b" }""") should === ("AB")
    typeNameFromJson("""{ "num": 3.14  }""") should === ("Num314")
    typeNameFromJson("""{ "person": { "name": "Bob", "street": 34 }  }""") should === ("PersonNameBobStreet34")
  }

}
