package argus.macros

import scala.reflect.api.Universe

/**
  * @author Aish Fenton.
  */
trait CodecBuilder {

  val u: Universe
  import u._

  val helpers = new ASTHelpers[u.type](u)
  import helpers._

  def mkEncoder(path: List[String], typ: TypeName, params: List[Tree]): Tree
  def mkDecoder(path: List[String], typ: TypeName, params: List[Tree]): Tree

  def inEncoder(typ: Tree): Tree
  def inDecoder(typ: Tree): Tree

  def mkEncoderValDef(path: List[String], typ: TypeName, params: List[Tree]): Tree = {
    val name = TermName(path.mkString + typ.toString + "Encoder")
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)
    val encDef = mkEncoder(path, typ, params)
    q"implicit val $name: ${inEncoder(fullTyp)} = $encDef"
  }

  def mkDecoderValDef(path: List[String], typ: TypeName, params: List[Tree]): Tree = {
    val name = TermName(path.mkString + typ.toString + "Decoder")
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)
    val decDef = mkDecoder(path, typ, params)
    q"implicit val $name: ${inDecoder(fullTyp)} = $decDef"
  }

  def mkCodec(defs: List[Tree], path: List[String] = Nil): List[Tree] = {
    val codecs = defs collect {

      case (q"$mods class $name[..$tparams] $ctorMods(..$params) extends { ..$earlydefns } with ..$parents { $self => ..$stats }") if mods.hasFlag(Flag.CASE) => {
        mkEncoderValDef(path, name, params) :: mkDecoderValDef(path, name, params) :: Nil
      }

      // If object defined, then recurse into it
      case (objDef @ q"$mods object $name { ..$body }") => mkCodec(body, path :+ name.toString)

    } flatten

    codecs
  }

}
