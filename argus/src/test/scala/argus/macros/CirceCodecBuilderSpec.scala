package argus.macros

import org.scalatest.{FlatSpec, Matchers}
import scala.reflect.api.Universe

/**
  * @author Aish Fenton.
  */
class CirceCodecBuilderSpec extends FlatSpec with Matchers with ASTMatchers {

  import runtimeUniverse._

  val codecBuilder = new CirceCodecBuilder[runtimeUniverse.type](runtimeUniverse)

  "mkEncoder()" should "make encoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int, b: String, c: Foo.Root)"
    codecBuilder.mkEncoder(Nil, name, params) should === (q"""
      io.circe.Encoder.instance((test: Test) => Json.obj(
        "a" -> test.a.asJson,
        "b" -> test.b.asJson,
        "c" -> test.c.asJson
      ))
    """)
  }

  it should "reflect the path in the encoder's types" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkEncoder(List("Foo"), name, params) should === (q"""
      io.circe.Encoder.instance((test: Foo.Test) => Json.obj( "a" -> test.a.asJson))
    """)
  }

  "mkEncoderValDef()" should "make an implicit val to an encoder" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"

    codecBuilder.mkEncoderValDef(Nil, name, params) should === (q"""
      implicit val TestEncoder: io.circe.Encoder[Test] = io.circe.Encoder.instance((test: Test) => Json.obj("a" -> test.a.asJson))
    """)
  }

  it should "reflect the path in the encoder's ValDef" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkEncoderValDef(List("Foo", "Bar"), name, params) should === (q"""
      implicit val FooBarTestEncoder: io.circe.Encoder[Foo.Bar.Test] = io.circe.Encoder.instance((test: Foo.Bar.Test) => Json.obj("a" -> test.a.asJson))
    """)
  }

  "mkDecoder()" should "make decoders for a given case class" in {
    val q"case class $name (..$params)" = q"case class Test(a: Option[Int], b: String, c: Option[Foo.Root])"
    codecBuilder.mkDecoder(Nil, name, params) should === (q"""
      io.circe.Decoder.instance((c: HCursor) => for {
        a <- c.downField("a").as[Option[Int]]
        b <- c.downField("b").as[String]
        c <- c.downField("c").as[Option[Foo.Root]]
      } yield Test(a,b,c))
    """)
  }

  it should "reflect the path in decoder's types" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkDecoder(List("Foo"), name, params) should === (q"""
      io.circe.Decoder.instance((c: HCursor) => for { a <- c.downField("a").as[Int] } yield Foo.Test(a))
    """)
  }

  "mkDecoderValDef()" should "make an implicit val to an decoder" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"

    codecBuilder.mkDecoderValDef(Nil, name, params) should === (q"""
      implicit val TestDecoder: io.circe.Decoder[Test] = io.circe.Decoder.instance((c: HCursor) => for { a <- c.downField("a").as[Int] } yield Test(a))
    """)
  }

  it should "reflect the path in the decoder's ValDef" in {
    val q"case class $name (..$params)" = q"case class Test(a: Int)"
    codecBuilder.mkDecoderValDef(List("Foo"), name, params) should === (q"""
      implicit val FooTestDecoder: io.circe.Decoder[Foo.Test] = io.circe.Decoder.instance((c: HCursor) => for { a <- c.downField("a").as[Int] } yield Foo.Test(a))
    """)
  }

  "mkCodec()" should "make add an encoder/decoder for each case class" in {
    val defs = List(
      q"case class Foo(a: Int)",
      q"case class Bar(b: Int, c: String)"
    )
    val res = codecBuilder.mkCodec(defs)

    res should have size (4)
    res map extractCodecNameAndType should contain theSameElementsAs
      List(("FooEncoder","io.circe.Encoder[Foo]"), ("FooDecoder","io.circe.Decoder[Foo]"), ("BarEncoder","io.circe.Encoder[Bar]"), ("BarDecoder","io.circe.Decoder[Bar]"))
  }

  it should "ignore other definitions" in {
    val defs = List(
      q"val a = 1",
      q"object A"
    )
    val res = codecBuilder.mkCodec(defs)
    res should be (empty)
  }

  it should "work with nested case classes" in {
    val defs = List(q"object Root { object Foo { case class Bar(a: Int) } }")
    val res = codecBuilder.mkCodec(defs)
    res map extractCodecNameAndType should contain theSameElementsAs
      List(("RootFooBarEncoder","io.circe.Encoder[Root.Foo.Bar]"), ("RootFooBarDecoder","io.circe.Decoder[Root.Foo.Bar]"))
  }

}
