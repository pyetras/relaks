package relaks.optimizer

import scala.util.Try

/**
 * Created by Pietras on 07/05/15.
 */

//TODO boo wendy booo
case class TypeDesc(`type`: String, min: String, max: String)
case class EnumTypeDesc(options: Seq[String])

object NondetParamExtensions {
  trait Spearmint extends { this: NondetParams with SpearmintOptimizer =>
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.DefaultFormats
    implicit val formats = DefaultFormats

    implicit class SpaceJsonOps(self: ParamsSpace) {
      def toSpearmintJson: JObject = {
        self.map(nondetParamDefToJObject).reduceLeft[JObject](_ ~ _)
      }
      def fromSpearmintJson(json: String): ParamsSpace = ???
      def paramsFromSpearmintResults(res: Array[String]): Try[Params] = Try {
        val p = res.drop(2)
        assert(p.length == self.size, "wrong number of parameters")
        self.toSeq.zip(p).map(kvp => kvp._1._1 -> kvp._1._2.fromString(kvp._2)).toMap
      }
    }

    implicit class ParamProviderJsonOps(self: ParamProvider) {
      def toSpearmintJson = self.paramSpace.toSpearmintJson
    }

    implicit class NondetParamTypeJsonOps(self: NondetParam[Any]) {

      def toSpearmintJson: JObject = ("size" -> 1) ~ (self match {
        case ChooseOneOf(seq) =>
          ("type" -> "enum") ~ Extraction.decompose(EnumTypeDesc(seq.indices.map(_.toString))).asInstanceOf[JObject]
        case DiscreteRange(from, to, step) if step != 1 =>
          Extraction.decompose(TypeDesc("int", "0", Range(from, to, step).length.toString)).asInstanceOf[JObject]
        case RangeLikeSpace(from, to) =>
          Extraction.decompose(TypeDesc(mapTypeName(), from.toString, to.toString)).asInstanceOf[JObject]
      })

      def toStringResult(v: Any): String = self match {
        case ChooseOneOf(seq) => seq.indexOf(v).toString
        case DiscreteRange(from, to, step) => ((v.asInstanceOf[Int] - from)/step).toString
        case _ => v.toString
      }

      def fromString(str: String): Any = self match {
        case ChooseOneOf(seq) => seq(str.toInt)
        case RangeLikeSpace(from, to) if mapTypeName() == "float" =>
          val d = str.toDouble
          assert(d <= to.asInstanceOf[Double] && d >= from.asInstanceOf[Double], "parameter not within bounds")
          d
        case DiscreteRange(from, to, step) if step != 1 =>
          val i = from + str.toInt*step
          assert(i <= to && i >= from, "parameter not within bounds")
          i
        case RangeLikeSpace(from, to)  if mapTypeName() == "int" =>
          val i = str.toInt
          assert(i <= to.asInstanceOf[Int] && i >= from.asInstanceOf[Int], "parameter not within bounds")
          i
      }

      private def mapTypeName(): String = self.typeTag.dealias.toString match {
        case "Int" => "int"
        case "Double" => "float"
        case "Float" => "float"
        case t@_ => throw new NotImplementedError(s"Type $t not implemented in spearmint")
      }
    }

    private def nondetParamDefToJObject[A](self: (String, NondetParam[A])): JObject =
      self._1 -> (("name" -> self._1) ~ self._2.toSpearmintJson)
  }
}
