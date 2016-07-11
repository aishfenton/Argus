package argus

import org.scalatest._

/**
  * @author Aish Fenton.
  */
class UnionTypeSpec extends FlatSpec with Matchers {
  import UnionType._

  case class Foo[A :  (UNil OrA Int)#check](a: A)
  case class Foo2[A :  (UNil OrA Int OrA String)#check](a: A)
  case class Foo3[A : (((UNil OrA Int) OrA String OrA List[Int]))#check](a: A)
//  case class Foo4[A : (UNil OrA Int OrA String OrA List[Int] OrA List[String])#check](a: A)
//  case class Foo5[A : (Int Or String Or List[Int] Or List[String] Or Symbol)#check](a: A)

  "UnionType" should "should work with singular values too" in {
    Foo(1).a should === (1)

    "Foo(\"a\")" shouldNot typeCheck
    "Foo(2.0)" shouldNot typeCheck
  }

  it should "enforce that passed in type is either A or B (or both) and not compile otherwise" in {
    Foo2(1).a should === (1)
    Foo2("a").a should === ("a")

    "Foo2(2.0)" shouldNot typeCheck
    "Foo2(List(1))" shouldNot typeCheck
  }

  it should "work with unions up to arity 5" in {
    Foo3(1).a should === (1)
    Foo3("a").a should === ("a")
    Foo3(List(1)).a === (List(1))

    "Foo3(2.0)" shouldNot typeCheck
    "Foo3(List(2.0))" shouldNot typeCheck
  }
}
