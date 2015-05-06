import relaks.data.syntax._
import shapeless.HList

val tup = fromTupleWithLabels((1 as "text", "string" as "int"))
tup.text
