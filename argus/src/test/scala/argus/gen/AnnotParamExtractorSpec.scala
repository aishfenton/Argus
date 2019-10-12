package argus.gen

import org.scalatest.{FlatSpec, Matchers}
import scala.meta._
import scala.meta.contrib._

class AnnotParamExtractorSpec extends FlatSpec with Matchers {

  "AnnotParamExtractor" should "extract params from an annotation" in {

    val annot = q"""
      @abc(true, 1, 2L, 3.0f, 4.0, 'a', "b", 'c) case class Test(i: Int)
    """.extract[Mod.Annot].head

    annot.extract[AnnotParams].head.getAll should === (
      List(true, 1, 2L, 3.0f, 4.0, 'a',  "b", 'c)
    )
  }

  it should "support named params" in {
    val annot = q"""
      @abc(1, b=2, 3, d="4") case class Test(i: Int)
    """.extract[Mod.Annot].head

    val params = annot.extract[AnnotParams].head

    params.getAs[Int](0, "a") should === (Some(1))
    params.getAs[Int](1, "b") should === (Some(2))
    params.getAs[String](3, "d") should === (Some("4"))
  }

  it should "support optional wrappers" in {
    val annot = q"""
      @abc(Some(1), None, c=Some("a")) case class Test(i: Int)
    """.extract[Mod.Annot].head

    val params = annot.extract[AnnotParams].head

    params.getAs[Option[Int]](0, "a") should === (Some(Some(1)))
    params.getAs[Option[Int]](1, "b") should === (Some(None))
    params.getAs[Option[String]](2, "c") should === (Some(Some("a")))
  }

  it should "support seq values" in {
    val annot = q"""
      @abc(Seq(1,2,3), 4, c=List("a", "b")) case class Test(i: Int)
    """.extract[Mod.Annot].head

    val params = annot.extract[AnnotParams].head

    params.getAs[Seq[Int]](0, "a") should === (Some(Seq(1, 2, 3)))
    params.getAs[List[String]](2, "c") should === (Some(List("a", "b")))
  }

  it should "support multiple argument lists" in {
    val annot = q"""
      @abc(1, b=2)(3, d="4") case class Test(i: Int)
    """.extract[Mod.Annot].head

    val params = annot.extract[AnnotParams]

    params.head.getAll should === (List(1, 2))
    params.last.getAll should === (List(3, "4"))

  }

}
