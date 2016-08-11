package argus.macros

import scala.reflect.api.Universe

/**
  * @author Aish Fenton.
  */
class CirceCodecBuilder[U <: Universe](val u: U) extends CodecBuilder {

  import u._
  import helpers._

  val imports = q"import io.circe._"  :: q"import io.circe.syntax._" :: Nil

  def inEncoder(typ: Tree) = tq"Encoder[$typ]"

  def inDecoder(typ: Tree) = tq"Decoder[$typ]"

  def mkUnionEncoder(path: List[String], typ: TypeName, subTypes: List[(Tree, Tree)]): Tree = {
    val caseDefs = subTypes.map { case(rawType, unionType) =>
      cq"ut: $unionType => ut.x.asJson"
    }

    val encDef = q"""
    Encoder.instance { case ..$caseDefs }
    """

    encDef
  }

  def mkUnionDecoder(path: List[String], typ: TypeName, subTypes: List[(Tree, Tree)]): Tree = {
    val (rt, ut) = subTypes.head
    val asDefs: Tree = subTypes.tail.foldLeft(q"c.as[$rt].map((x) => ${typeNameToTermName(ut)}(x))") {
      case (s:Tree, (rt:Tree, ut: Tree)) => q"$s.orElse(c.as[$rt].map((x) => ${typeNameToTermName(ut)}(x)))"
    }

    q"""
    Decoder.instance((c: HCursor) => { $asDefs })
    """
  }

  def mkEnumEncoder(path: List[String], typ: TypeName, subTermPairs: List[(String, Tree)]): Tree = {
    // We can ignore subtypes and just encode based on supertype here
    q"""Encoder.instance(e => parser.parse(e.json).toOption.get)"""
  }

  def mkEnumDecoder(path: List[String], typ: TypeName, subTermPairs: List[(String, Tree)]): Tree = {
    val caseDefs = subTermPairs.map { case(jsonStr, subTerm) =>
      cq"j if j == parser.parse($jsonStr).toOption.get => cats.data.Xor.right($subTerm)"
    }

    val decDef = q"""
    Decoder.instance((c: HCursor) => for {
      json <- c.as[Json]
      singleton <- json match { case ..$caseDefs; case _ => throw new Exception("Couldn't find enum:" + json.toString) }
    } yield singleton)
    """

    decDef
  }

  /**
    * Builds an Json encoder expression using Circe
    * @param path For type that are nested within objects, path to them
    * @param typ The typ of the class that is encoded
    * @param params The parameters of the class
    */
  def mkEncoder(path: List[String], typ: TypeName, params: List[Tree]): Tree = {
    val valTerm = TermName(typeToName(typ))
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)

    val mappings = params.map { case q"$mods val $name: $tname = $default" =>
      q""" ${name.toString} -> $valTerm.$name.asJson  """
    }

    val encDef = q"""
    Encoder.instance(($valTerm: $fullTyp) => Json.obj( ..$mappings ))
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
    val (forVals, vals) = params.foldLeft((List[Tree](), List[TermName]())) {
      case ((es, vs), q"$mods val $name: $tname = $default") =>
        (es :+ fq"""$name <- c.downField(${name.toString}).as[$tname]""", vs :+ name)
    }

    val fullTermName = mkSelectPath(path :+ typ.toString)
    val decDef = q"""
    Decoder.instance((c: HCursor) => for(..$forVals) yield $fullTermName(..$vals))
    """

    decDef
  }

}
