package argus.json

import org.scalatest.{FlatSpec, Matchers}
import io.circe.parser._
import java.net.URI

import io.circe.Json

class JsonPointerSpec extends FlatSpec with Matchers {

  val json = parse( """
  {
    "foo" : "bar",
    "arr" : [ { "a" : 1 }, { "a" : 2 } ],
    "nested" : { "a" : { "b" : 1 } }
  }
  """).toOption.get

  "JsonPointer" should "support relative and absolute paths" in {
    JsonPointer.fromString("/foo").relative should === (false)
    JsonPointer.fromString("foo/a").relative should === (true)
  }

  it should "use given field names" in {
    JsonPointer.fromString("/foo/bar/baz/0").tokens should === (List("foo", "bar", "baz", "0"))
    JsonPointer.fromString("foo//0/bar").tokens should === (List("foo", "0", "bar"))
  }

  it should "unescape escaped names" in {
    JsonPointer.fromString("/a~1b/c").tokens should === (List("a/b", "c"))
    JsonPointer.fromString("/a~0b/c~d").tokens should === (List("a~b", "c~d"))
    JsonPointer.fromString("/a~01/c").tokens should === (List("a~1", "c"))
    JsonPointer.fromString("/a~10/c").tokens should === (List("a/0", "c"))
  }

  it should "support building from a URI" in {
    val ptr = JsonPointer.fromURI(new URI("http://example.com/a/b/c#/0/1/2"))
    ptr should === (JsonPointer(List("0", "1", "2"), false))
  }

  "JsonPointerTraverser" should "traverse an absolute path" in {
    val c = JsonPointerTraverser(json, JsonPointer.fromString("/foo"))
    c.toOption.flatMap(_.focus) should === (Some(Json.fromString("bar")))

    val c2 = JsonPointerTraverser(json, JsonPointer.fromString("/arr/1/a"))
    c2.toOption.flatMap(_.focus) should === (Some(Json.fromInt(2)))

    val c3 = JsonPointerTraverser(json, JsonPointer.fromString("/nested/a/b"))
    c3.toOption.flatMap(_.focus) should === (Some(Json.fromInt(1)))
  }

  it should "traverse a relative position" in {
    val j2 = json.hcursor.downField("arr").downN(0).focus.get

    val c = JsonPointerTraverser(j2, JsonPointer.fromString("a"))
    c.toOption.flatMap(_.focus) should === (Some(Json.fromInt(1)))
  }

}
