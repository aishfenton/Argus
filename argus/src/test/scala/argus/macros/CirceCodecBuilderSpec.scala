package argus.macros

import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.api.Universe

class CirceCodecBuilderSpec extends FlatSpec with Matchers with ASTMatchers {

  import runtimeUniverse._

  val codecBuilder = new CirceCodecBuilder[runtimeUniverse.type](runtimeUniverse)

  "mkEncoder()" should "make encoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int, b: String, c: Foo.Root)"
    codecBuilder.mkEncoder(Nil, name, params) should === (q"""
      Encoder.instance((test: Test) => Json.obj(
        "a" -> test.a.asJson,
        "b" -> test.b.asJson,
        "c" -> test.c.asJson
      ))
    """)
  }

  it should "reflect the path in the encoder's types" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkEncoder(List("Foo"), name, params) should === (q"""
      Encoder.instance((test: Foo.Test) => Json.obj( "a" -> test.a.asJson))
    """)
  }

  "mkEncoderValDef()" should "make an implicit val to an encoder" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"

    codecBuilder.mkEncoderValDef(Nil, name, q"???") should === (q"""
      implicit val TestEncoder: Encoder[Test] = ???
    """)
  }

  it should "reflect the path in the encoder's ValDef" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkEncoderValDef(List("Foo", "Bar"), name, q"???") should === (q"""
      implicit val FooBarTestEncoder: Encoder[Foo.Bar.Test] = ???
    """)
  }

  "mkDecoder()" should "make decoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Option[Int], b: String, c: Option[Foo.Root])"
    codecBuilder.mkDecoder(Nil, name, params) should === (q"""
      Decoder.instance((c: HCursor) => for {
        a <- c.downField("a").as[Option[Int]]
        b <- c.downField("b").as[String]
        c <- c.downField("c").as[Option[Foo.Root]]
      } yield Test(a,b,c))
    """)
  }

  it should "reflect the path in decoder's types" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkDecoder(List("Foo"), name, params) should === (q"""
      Decoder.instance((c: HCursor) => for { a <- c.downField("a").as[Int] } yield Foo.Test(a))
    """)
  }

  "mkDecoderValDef()" should "make an implicit val to an decoder" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"

    codecBuilder.mkDecoderValDef(Nil, name, q"???") should === (q"""
      implicit val TestDecoder: Decoder[Test] = ???
    """)
  }

  it should "reflect the path in the decoder's ValDef" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkDecoderValDef(List("Foo"), name, q"???") should === (q"""
      implicit val FooTestDecoder: Decoder[Foo.Test] = ???
    """)
  }

  "mkUnionEncoder()" should "make encoders for a given @union set" in {
    val union =
      (tq"Int", tq"FooInt") ::
      (tq"String", tq"Root.FooString") ::
      (tq"Bar.Person", tq"FooBarPerson") ::
      Nil

    val res = codecBuilder.mkUnionEncoder(Nil, TypeName("Foo"), union)

    res should === (q"""
      Encoder.instance {
        case ut: FooInt => ut.x.asJson
        case ut: Root.FooString => ut.x.asJson
        case ut: FooBarPerson => ut.x.asJson
      }
    """)
  }

  "mkUnionDecoder()" should "make decoders for a given @union set" in {
    val union =
      (tq"Int", tq"FooInt") ::
      (tq"String", tq"Root.FooString") ::
      (tq"Bar.Person", tq"FooBarPerson") ::
      Nil

    val res = codecBuilder.mkUnionDecoder(Nil, TypeName("Foo"), union)

    res should === (q"""
      Decoder.instance((c: HCursor) => {
        val res = c.as[Int] orElse c.as[String] orElse c.as[Bar.Person]
        res.bimap(
          (f) => DecodingFailure("Couldn't decode Items: " + c.focus.toString, f.history),
          {
            case rt: Int => FooInt(rt)
            case rt: String => Root.FooString(rt)
            case rt: Bar.Person => FooBarPerson(rt)
            case rt@_ => throw new Exception("Don't know typ " + rt)
          }
        )
      })
    """)
  }

  "mkEnumEncoder()" should "make encoders for a given @enum set" in {
    val enums =
      ("1", tq"Foo1") ::
      ("\"NZ\"", tq"FooNZ") ::
      Nil

    val res = codecBuilder.mkEnumEncoder(Nil, TypeName("Foo"), enums)
    res should === (q"""
      Encoder.instance(e => parser.parse(e.json).toOption.get)
    """)
  }

  "mkEnumDecoder()" should "make decoders for a given @enum set" in {
    val pairs =
      ("1", q"FooEnum.Foo1") ::
      ("\"NZ\"", q"FooEnum.FooNZ") ::
      Nil

    val res = codecBuilder.mkEnumDecoder(Nil, TypeName("Foo"), pairs)
    res should === (q"""
      Decoder.instance((c: HCursor) => for {
        json <- c.as[Json]
        singleton <- json match {
          case j if j == parser.parse("1").toOption.get => cats.data.Xor.right(FooEnum.Foo1)
          case j if j == parser.parse("\"NZ\"").toOption.get => cats.data.Xor.right(FooEnum.FooNZ)
          case _ => throw new Exception("Couldn't find enum:" + json.toString)
        }
      } yield singleton)
    """)
  }

  "mkCodec()" should "make add an encoder/decoder for each case class" in {
    val defs =
      q"case class Foo(a: Int)" ::
      q"case class Bar(b: Int, c: String)" ::
      Nil

    val res = codecBuilder.mkCodec(defs)

    res collect extractCodecNameAndType should contain theSameElementsAs
      List(("FooEncoder","Encoder[Foo]"), ("FooDecoder","Decoder[Foo]"), ("BarEncoder","Encoder[Bar]"), ("BarDecoder","Decoder[Bar]"))
  }

  it should "ignore other definitions" in {
    val defs =
      q"val a = 1" ::
      q"object A" ::
      Nil

    val res = codecBuilder.mkCodec(defs) collect extractCodecNameAndType
    res should be (empty)
  }

  it should "work with nested case classes" in {
    val defs = List(q"object Root { object Foo { case class Bar(a: Int) } }")
    val res = codecBuilder.mkCodec(defs)
    res collect extractCodecNameAndType should contain theSameElementsAs
      List(("RootFooBarEncoder","Encoder[Root.Foo.Bar]"), ("RootFooBarDecoder","Decoder[Root.Foo.Bar]"))
  }

  it should "add an encoder/decoder for each @enum" in {
    val defs =
      q"@enum sealed trait Foo extends scala.Product with scala.Serializable { def json: String }" ::
      q"""
        object FooEnum {
          case object Foo1 extends Foo { val json: String = "1" }
          case object FooNZ extends Foo { val json: String = "\"NZ\"" }
        }
      """ ::
      Nil

    val res = codecBuilder.mkCodec(defs)

    res collect extractCodecNameAndType should contain theSameElementsAs
      ("FooEncoder","Encoder[Foo]") ::
      ("FooDecoder","Decoder[Foo]") ::
      Nil

    val code = res map(showCode(_)) mkString("")
    code should include("parse(\"1\")")
    code should include("""parse("\"NZ\"")""")
  }

  it should "add an encoder/decoder for each @union" in {
    val defs =
      q"@union sealed trait Foo extends scala.Product with scala.Serializable" ::
      q"case class FooInt(x: Int) extends Foo" ::
      q"case class FooBarPerson(x: bar.Person) extends Foo" ::
      Nil

    val res = codecBuilder.mkCodec(defs)
    res collect extractCodecNameAndType should contain theSameElementsAs
      ("FooEncoder","Encoder[Foo]") ::
      ("FooDecoder","Decoder[Foo]") ::
      Nil

    val code = res map(showCode(_)) mkString("")
    code should include("FooBarPerson(rt)")
    code should include("FooInt(rt)")
  }

}
