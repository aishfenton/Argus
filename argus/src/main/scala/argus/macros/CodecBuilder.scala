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

  /**
    * Imports to place at start of generated code (i.e. io.circe._ )
    */
  def imports: List[Tree]

  def mkEncoder(path: List[String], typ: TypeName, params: List[Tree]): Tree

  def mkDecoder(path: List[String], typ: TypeName, params: List[Tree]): Tree

  def inEncoder(typ: Tree): Tree

  def inDecoder(typ: Tree): Tree

  /**
    * Makes encoder instance definition for encoding enum types (without assigning it to a val)
    *
    * @param path         Path containing the enum
    * @param typ          The super type that is used to group all the enums
    * @param subTermPairs A list of tuples containing the jsonStr (which encodes the value of the enum) and the
    *                     TermName (which encodes the singleton of the enum).
    */
  def mkEnumEncoder(path: List[String], typ: TypeName, subTermPairs: List[(String, Tree)]): Tree

  /**
    * Makes decoder instance definition for decoding enum types (without assigning it to a val)
    *
    * @param path         Path containing the enum
    * @param typ          The super type that is used to group all the enums
    * @param subTermPairs A list of tuples containing the jsonStr (which encodes the value of the enum) and the
    *                     TermName (which encodes the singleton of the enum).
    */
  def mkEnumDecoder(path: List[String], typ: TypeName, subTermPairs: List[(String, Tree)]): Tree

  /**
    * Makes encoder instance definition for encoding union types (without assigning it to a val)
    *
    * @param path     Path containing the enum
    * @param typ      The super type that is used to group all the enums
    * @param subTypes A list of tuples containing the rawType (i.e. Int) and the union wrapper type (i.e. FooInt)
    */
  def mkUnionEncoder(path: List[String], typ: TypeName, subTypes: List[(Tree, Tree)]): Tree

  /**
    * Makes decoder instance definition for decoding union types (without assigning it to a val)
    *
    * @param path     Path containing the enum
    * @param typ      The super type that is used to group all the enums
    * @param subTypes A list of tuples containing the rawType (i.e. Int) and the union wrapper type (i.e. FooInt)
    */
  def mkUnionDecoder(path: List[String], typ: TypeName, subTypes: List[(Tree, Tree)]): Tree


  def mkEncoderValDef(path: List[String], typ: TypeName, encDef: Tree): Tree = {
    val name = TermName(path.mkString + typ.toString + "Encoder")
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)
    q"implicit val $name: ${inEncoder(fullTyp)} = $encDef"
  }

  def mkDecoderValDef(path: List[String], typ: TypeName, decDef: Tree): Tree = {
    val name = TermName(path.mkString + typ.toString + "Decoder")
    val fullTyp = mkTypeSelectPath(path :+ typ.toString)
    q"implicit val $name: ${inDecoder(fullTyp)} = $decDef"
  }

  def mkCodec(defs: List[Tree], path: List[String] = Nil): List[Tree] = {

    // Needed so that we can exclude case classes that extend these from normal handling
    val unionTypes = defs collect { case q"$mods trait $typ extends ..$_" if mods.hasFlag(Flag.SEALED) &&
      mods.annotations.exists(_.equalsStructure(q"new union()")) =>

      Ident(typ)
    }

    def mkCodecRec(defs: List[Tree], path: List[String], unionTypes: List[Tree]): List[Tree] = defs collect {

      //----
      // Enums
      //----
      case (q"$mods trait $typ extends ..$_ { ..$_ }") if mods.hasFlag(Flag.SEALED) &&
        mods.annotations.exists(_.equalsStructure(q"new enum()")) => {

        // Extract details of case object
        val members = extendsType(path, Ident(typ), defs).map { case(path, subDef) =>
          val q"case object $name extends $_ { val json: String = $jsonLit }" = subDef
          val Literal(Constant(jsonStr: String)) = jsonLit
          (jsonStr, mkSelectPath(path :+ name.toString))
        }

        val encDef = mkEnumEncoder(path, typ, members)
        val decDef = mkEnumDecoder(path, typ, members)
        mkEncoderValDef(path, typ, encDef) :: mkDecoderValDef(path, typ, decDef) :: Nil
      }

      //----
      // Union types
      //----
      case (q"$mods trait $typ extends ..$_") if mods.hasFlag(Flag.SEALED) &&
        mods.annotations.exists(_.equalsStructure(q"new union()")) => {

        // Extract details of case object
        val members = extendsType(path, Ident(typ), defs).map { case(path, subDef) =>
          val q"case class $unionType(x: $rawType) extends $_" = subDef
          (rawType, mkTypeSelectPath(path :+ unionType.toString))
        }

        val encDef = mkUnionEncoder(path, typ, members)
        val decDef = mkUnionDecoder(path, typ, members)
        mkEncoderValDef(path, typ, encDef) :: mkDecoderValDef(path, typ, decDef) :: Nil
      }

      //----
      // Case classes
      //----
      case (q"$mods class $name[..$tparams] $ctorMods(..$params) extends { ..$_ } with ..$parents")
        if mods.hasFlag(Flag.CASE) && treesIntersect(parents, unionTypes).isEmpty => {

        val encDef = mkEncoder(path, name, params)
        val decDef = mkDecoder(path, name, params)
        mkEncoderValDef(path, name, encDef) :: mkDecoderValDef(path, name, decDef) :: Nil
      }

      // If object defined, then recurse into it
      case (objDef@q"$mods object $name { ..$body }") => mkCodecRec(body, path :+ name.toString, unionTypes)

    } flatten

    imports ++ mkCodecRec(defs, path, unionTypes)
  }


}
