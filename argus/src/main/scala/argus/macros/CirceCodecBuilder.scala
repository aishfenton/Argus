package argus.macros

import scala.reflect.api.Universe

/**
  * @author Aish Fenton.
  */
class CirceCodecBuilder[U <: Universe](val u: U) extends CodecBuilder {

  import u._
  import helpers._

  def inEncoder(typ: Tree) = tq"io.circe.Encoder[$typ]"

  def inDecoder(typ: Tree) = tq"io.circe.Decoder[$typ]"

  /**
    * Builds an Json encoder expression using Circe
    * @param path For type that are nested within objects, path to them
    * @param typ The typ of the class that is encoded
    * @param params The parameters of the class
    */
  def mkEncoder(path: List[String], typ: TypeName, params: List[Tree]): Tree = {
    val valTerm = typeToTermName(typ)
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)

    val mappings = params.map { case q"$mods val $name: $tname = $default" =>
      q""" ${name.toString} -> $valTerm.$name.asJson  """
    }

    val encDef = q"""
    io.circe.Encoder.instance(($valTerm: $fullTyp) => Json.obj( ..$mappings ))
    """

    encDef
  }

  /**
    * Builds an Json decoder expression using Circe
    * @param path For type that are nested within objects, path to them
    * @param typ The typ of the class that is decoded
    * @param params The parameters of the class
    */
  def mkDecoder(path: List[String], typ: TypeName, params: List[Tree]): Tree = {
    val (enums, vals) = params.foldLeft((List[Tree](), List[TermName]())) {
      case ((es, vs), q"$mods val $name: $tname = $default") =>
        (es :+ fq"""$name <- c.downField(${name.toString}).as[$tname]""", vs :+ name)
    }

    val fullTermName = mkSelectPath(path :+ typ.toString)
    val decDef = q"""
    io.circe.Decoder.instance((c: HCursor) => for(..$enums) yield $fullTermName(..$vals))
    """

    decDef
  }

}
