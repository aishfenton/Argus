package argus.macros

import scala.language.experimental.macros
import scala.reflect.api.Universe

private[macros] object ParamsASTHelper {

  def paramsToMap[U <: Universe](u: U)(
      nameAndDefaults: List[(String, Any)],
      params: List[u.Tree]): Map[String, Any] = {
    import u._

    def toValue(param: Tree): Any = param match {
      case Ident(TermName("None")) => None
      case Apply(Ident(TermName("Some")), List(Literal(Constant(value)))) =>
        Some(value)
      case Literal(Constant(c)) => c
      case _                    => param
    }

    // Handle positional vs. named argument separately
    val (positional, named) = params.splitAt(params.segmentLength {
      case NamedArg(Ident(TermName(_)), _) => false
      case Assign(Ident(TermName(_)), _)   => false
      case _                               => true
    })

    require(positional.length <= nameAndDefaults.length,
            "More position arguments than specified in: " + nameAndDefaults)
    val posValues = nameAndDefaults
      .zip(positional)
      .map {
        case ((name, default), param) =>
          (name, toValue(param))
      }
      .toMap

    val namedValues = named.map {
      case NamedArg(Ident(TermName(name)), t: Tree) => (name, toValue(t))
      case Assign(Ident(TermName(name)), t: Tree)   => (name, toValue(t))
    }.toMap

    nameAndDefaults.toMap ++ posValues ++ namedValues
  }

}
