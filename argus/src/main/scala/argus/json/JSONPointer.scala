package argus.json

import java.net.URI

import io.circe._

import scala.annotation.tailrec
import scala.util.Try

sealed trait JsonPointer

/**
  * Implements [[https://tools.ietf.org/html/rfc6901]] and [[https://json-schema.org/latest/relative-json-pointer.html]]
  */
case class AbsoluteJsonPointer(path: List[String]) extends JsonPointer {

  /**
    * Converts the pointer to a "canonical form". Which we define here to be:
    *
    * - If the element has it's own $id then this is as the path, after the last parent base URI.
    * - The absolute path to the last base URI on path to root.
    *
    * See [[https://json-schema.org/latest/json-schema-core.html#idExamples]]
    */
  def toCanonicalURI(json: Json): URI = {
    ???
  }

}

case class RelativeJsonPointer(parentUps: Int, selectIdent: Boolean, jsonPointer: AbsoluteJsonPointer) extends JsonPointer

object RelativeJsonPointer {

  val Parts = """^(?:(?<ups>\d+)(?<indent>#?))?(?<ptr>.+)""".r

  private def isEmptyOrNull(str: String) = str == null || str.isEmpty

  def fromString(str: String): Option[RelativeJsonPointer] = {
    str match {
      case Parts(ups, ident, ptr) if Try(ups.toInt).isSuccess => {
        RelativeJsonPointer(ups.toInt, !isEmptyOrNull(ident), AbsoluteJsonPointer.fromString(ptr))
      }
      case _ => RelativeJsonPointer(0, false, AbsoluteJsonPointer.fromString(str))
    }
  }

}


object AbsoluteJsonPointer {
  val Format = """^(.+)""".r
  val Delimiter = """^(\/.+)*$""".r

  private def unescape(part: String) = part
    .trim
    .replaceAll("~1", "/")
    .replaceAll("~0", "~")

  private def isValidateStr(str: String) = Format.findFirstIn(str).isDefined

  /**
    * Makes a JsonPointer from a string representation, such as "/a/b/c/1/a"
    */
  def fromString(str: String): Option[AbsoluteJsonPointer] = if (isValidateStr(str)) {
    val selectors = str
      .trim
      .split(Delimiter)
      .toList

    Some(AbsoluteJsonPointer(selectors))
  } else {
    None
  }

  /**
    * Makes a JSONPointer from a URI. The JsonPointer is only contained on the fragment part of the URI.
    */
  def fromURI(uri: URI) =   fromString(uri.getFragment)

}

/**
  * Traverses a given (Circe) Json object using a JsonPointer
  */
object JsonPointerTraverser {

  case class PointerFailure(currentCursor: Option[Json], token: String)
  type Result = Either[PointerFailure, ACursor]

  def apply(json: Json, ptr: JsonPointer): Result = {
    assert(json.isObject, "Traversal must start from a Json object")

    @tailrec
    def go(c: ACursor, ts: List[String]): Result = (c.focus, ts) match {
      // base case
      case (_, Nil) => Right(c)

      case (Some(o: Json), s :: _) if o.isObject => go( c.downField(s), ts.tail )

      case (Some(a: Json), s :: _) if a.isArray && Try(s.toInt).isSuccess => go( c.downN(s.toInt), ts.tail )

      // failure cases
      case _ => Left(PointerFailure(c.focus, ts.head))

    }

    go(json.hcursor, ptr.tokens)
  }

}
