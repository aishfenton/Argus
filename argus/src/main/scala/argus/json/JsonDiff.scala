package argus.json

import io.circe.Json

/**
  * @author Aish Fenton.
  */
object JsonDiff {

  def diff(j1: Json, j2: Json) = {
    val removeNulls = (t: (String, Json)) => { !t._2.isNull }

    def diffR(j1: Json, j2: Json): List[(String, String)] = {
      (j1.asObject, j2.asObject) match {
        case (Some(o1), Some(o2)) => {
          val o1m = o1.toMap.filter(removeNulls)
          val o2m = o2.toMap.filter(removeNulls)
          val sharedKeys = o1m.keySet intersect o2m.keySet

          // First record any missing fields
          val diffKeys =
            (o1m.keySet diff sharedKeys).map((_, "missing")) ++
            (o2m.keySet diff sharedKeys).map(("missing", _))

          // Now recurse on each field
          diffKeys.toList ++ sharedKeys.foldLeft(List[(String, String)]()) {
            case(accum, k) => accum ++ diffR(o1(k).get, o2(k).get)
          }
        }

        case _ => {
          (j1.asArray, j2.asArray) match {
            case (Some(a1), Some(a2)) => {
              a1.zip(a2).flatMap { case(jj1, jj2) => diffR(jj1,jj2) }
            }

            // Everything else
            case _ => if (j1 != j2) List((j1.toString, j2.toString)) else Nil
          }
        }
      }
    }

    diffR(j1, j2)
  }

}
