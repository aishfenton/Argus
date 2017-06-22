package argus.macros

import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.api.Universe

class CirceCodecBuilderSpec extends FlatSpec with Matchers with ASTMatchers {

  import runtimeUniverse._

  val codecBuilder = new CirceCodecBuilder[runtimeUniverse.type](runtimeUniverse)

  "mkEncoder()" should "make encoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int, b: String, c: Foo.Root)"
    codecBuilder.mkEncoder(Ident(name), params) should === (q"""
      Encoder.instance((cc: Test) => Json.obj(
        "a" -> cc.a.asJson,
        "b" -> cc.b.asJson,
        "c" -> cc.c.asJson
      ))
    """)
  }

  it should "reflect the path in the encoder's types" in {
    val (name, params) = (tq"Foo.Test", q"val a: Int")
    codecBuilder.mkEncoder(name, List(params)) should === (q"""
      Encoder.instance((cc: Foo.Test) => Json.obj("a" -> cc.a.asJson))
    """)
  }

  "mkEncoderValDef()" should "make an implicit val to an encoder" in {
    val name = tq"Foo.Bar.Test"

    codecBuilder.mkEncoderValDef(name, q"???") should === (q"""
      implicit val FooBarTestEncoder: Encoder[Foo.Bar.Test] = ???
    """)
  }

  "mkDecoder()" should "make decoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Option[Int], b: String, c: Option[Foo.Root])"
    codecBuilder.mkDecoder(Ident(name), params) should === (q"""
      Decoder.instance((c: HCursor) => for {
        a <- c.downField("a").as[Option[Int]]
        b <- c.downField("b").as[String]
        c <- c.downField("c").as[Option[Foo.Root]]
      } yield Test(a,b,c))
    """)
  }

  it should "reflect the path in decoder's types" in {
    val q"case class $_ (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkDecoder(tq"Foo.Test", params) should === (q"""
      Decoder.instance((c: HCursor) => for { a <- c.downField("a").as[Int] } yield Foo.Test(a))
    """)
  }

  "mkDecoderValDef()" should "make an implicit val to an decoder" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"

    codecBuilder.mkDecoderValDef(tq"Foo.Bar.Test", q"???") should === (q"""
      implicit val FooBarTestDecoder: Decoder[Foo.Bar.Test] = ???
    """)
  }

  "mkUnionEncoder()" should "make encoders for a given @union set" in {
    val union =
      (tq"Int", tq"FooInt") ::
        (tq"String", tq"Root.FooString") ::
        (tq"Bar.Person", tq"FooBarPerson") ::
        Nil

    val res = codecBuilder.mkUnionEncoder(tq"Foo", union)

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

    val res = codecBuilder.mkUnionDecoder(tq"Foo", union)

    res should === (q"""
      Decoder.instance((c: HCursor) => {
        c.as[Int].map((x) => FooInt(x)) orElse
        c.as[String].map((x) => Root.FooString(x)) orElse
        c.as[Bar.Person].map((x) => FooBarPerson(x))
      })
    """)
  }

  "mkEnumEncoder()" should "make encoders for a given @enum set" in {
    val enums =
      ("1", tq"Foo1") ::
        ("\"NZ\"", tq"FooNZ") ::
        Nil

    val res = codecBuilder.mkEnumEncoder(tq"Foo", enums)
    res should === (q"""
      Encoder.instance((e: Foo) => parser.parse(e.json).toOption.get)
    """)
  }

  "mkEnumDecoder()" should "make decoders for a given @enum set" in {
    val pairs =
      ("1", q"FooEnum.Foo1") ::
        ("\"NZ\"", q"FooEnum.FooNZ") ::
        Nil

    val res = codecBuilder.mkEnumDecoder(tq"Foo", pairs)
    res should === (q"""
      Decoder.instance((c: HCursor) => for {
        json <- c.as[Json]
        singleton <- json match {
          case j if j == parser.parse("1").toOption.get => Either.right(FooEnum.Foo1)
          case j if j == parser.parse("\"NZ\"").toOption.get => Either.right(FooEnum.FooNZ)
          case _ => Either.left(DecodingFailure("Couldn't find enum:" + json.toString, c.history))
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
      List(("dateTimeEncoder", "Encoder[org.joda.time.DateTime]"), ("dateTimeDecoder", "Decoder[org.joda.time.DateTime]"),
        ("FooEncoder","Encoder[Foo]"), ("FooDecoder","Decoder[Foo]"), ("BarEncoder","Encoder[Bar]"), ("BarDecoder","Decoder[Bar]"))
  }

  it should "ignore other definitions" in {
    val defs =
      q"val a = 1" ::
        q"object A" ::
        Nil

    val res = codecBuilder.mkCodec(defs) collect extractCodecNameAndType
    res should contain theSameElementsAs List(("dateTimeEncoder", "Encoder[org.joda.time.DateTime]"),
      ("dateTimeDecoder", "Decoder[org.joda.time.DateTime]"))
  }

  it should "work with nested case classes" in {
    val defs = List(q"object Root { object Foo { case class Bar(a: Int) } }")
    val res = codecBuilder.mkCodec(defs)
    res collect extractCodecNameAndType should contain theSameElementsAs
      List(("dateTimeEncoder", "Encoder[org.joda.time.DateTime]"), ("dateTimeDecoder", "Decoder[org.joda.time.DateTime]"),
        ("RootFooBarEncoder","Encoder[Root.Foo.Bar]"), ("RootFooBarDecoder","Decoder[Root.Foo.Bar]"))
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
      ("dateTimeEncoder", "Encoder[org.joda.time.DateTime]") ::
        ("dateTimeDecoder", "Decoder[org.joda.time.DateTime]") ::
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
      ("dateTimeEncoder", "Encoder[org.joda.time.DateTime]") ::
        ("dateTimeDecoder", "Decoder[org.joda.time.DateTime]") ::
        ("FooEncoder","Encoder[Foo]") ::
        ("FooDecoder","Decoder[Foo]") ::
        Nil

    val code = res map(showCode(_)) mkString("")
    code should include("FooBarPerson(x)")
    code should include("FooInt(x)")
  }

  it should "add an encoder/decoder for AnyWrapper types (and not for other types)" in {
    val defs =
      q"case class Foo(x: Any)" ::
        q"case class Bar(x: Int)" ::
        Nil

    val res = codecBuilder.mkCodec(defs)
    res collect extractCodecNameAndType should contain allOf(
      ("FooEncoder","Encoder[Foo]"),
      ("FooDecoder","Decoder[Foo]")
    )

    val code = res map(showCode(_)) mkString("")
    code should include("wrapper.x.asJson(anyEncoder)")
    code should include("h.as[Any](anyDecoder)")
  }

}
