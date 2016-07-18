package argus

import org.scalatest._

/**
  * @author Aish Fenton.
  */
class UnionTypeSpec extends FlatSpec with Matchers {
  import UnionType._

  case class Foo [A : (UNil Or Int)#check](a: A)
  case class Foo2[A : (UNil Or Int Or String)#check](a: A)
  case class Foo3[A : (UNil Or Int Or String Or List[Int])#check](a: A)
  case class Foo4[A : (UNil Or Int Or String Or List[Int] Or List[String])#check](a: A)
  case class Foo5[A : (UNil Or Int Or String Or List[Int] Or List[String] Or Symbol)#check](a: A)

  "UnionType" should "enforce that passed in type is either A or B (or both) and not compile otherwise" in {
    Foo(1).a should === (1)

    "Foo(\"a\")" shouldNot typeCheck
    "Foo(2.0)" shouldNot typeCheck
  }

  it should "should work with singular values too" in {
    Foo2(1).a should === (1)
    Foo2("a").a should === ("a")

    "Foo2(2.0)" shouldNot typeCheck
    "Foo2(List(1))" shouldNot typeCheck
  }

  it should "work with unions with >2 arity" in {
    Foo3(1).a should === (1)
    Foo3("a").a should === ("a")
    Foo3(List(1)).a === (List(1))
    "Foo3(2.0)" shouldNot typeCheck
    "Foo3(List(2.0))" shouldNot typeCheck

    Foo4(1).a should === (1)
    Foo4("a").a should === ("a")
    Foo4(List(1)).a === (List(1))
    Foo4(List("a")).a === (List("a"))
    "Foo4(2.0)" shouldNot typeCheck
    "Foo4('a)" shouldNot typeCheck

    Foo5(1).a should === (1)
    Foo5("a").a should === ("a")
    Foo5(List(1)).a === (List(1))
    Foo5(List("a")).a === (List("a"))
    Foo5('a).a === ('a)
    "Foo5(2.0)" shouldNot typeCheck
    "Foo5(List('a))" shouldNot typeCheck
  }
}
