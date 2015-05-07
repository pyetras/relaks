package relaks.optimizer

/**
 * Created by Pietras on 07/05/15.
 */

//TODO boo wendy booo
case class TypeDesc(`type`: String, min: String, max: String)
case class EnumTypeDesc(options: Seq[Int])

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
      def paramsFromSpearmintResults(res: Seq[String]): Params = ???
    }

    implicit class ParamProviderJsonOps(self: ParamProvider) {
      def toSpearmintJson = self.paramSpace.toSpearmintJson
    }

    implicit class NondetParamTypeJsonOps(self: NondetParamType[Any]) {

      def toSpearmintJson: JObject = ("size" -> 1) ~ (self match {
        case ChooseOneOf(seq) =>
          ("type" -> "enum") ~ Extraction.decompose(EnumTypeDesc(seq.indices)).asInstanceOf[JObject]
        case DiscreteRange(from, to, step) if step != 1 =>
          Extraction.decompose(TypeDesc("int", "0", Range(from, to, step).length.toString)).asInstanceOf[JObject]
        case RangeLikeSpace(from, to) =>
          Extraction.decompose(TypeDesc(mapTypeName(), from.toString, to.toString)).asInstanceOf[JObject]
      })

      private def mapTypeName(): String = self.typeTag.dealias.toString match {
        case "Int" => "int"
        case "Double" => "float"
        case "Float" => "float"
        case t@_ => throw new NotImplementedError(s"Type $t not implemented in spearmint")
      }
    }

    private def nondetParamDefToJObject[A](self: (String, NondetParamType[A])): JObject =
      self._1 -> (("name" -> self._1) ~ self._2.toSpearmintJson)
  }
}
