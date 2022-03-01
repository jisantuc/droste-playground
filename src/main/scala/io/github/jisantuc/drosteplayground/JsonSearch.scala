package io.github.jisantuc.drosteplayground

import higherkindness.droste.data.Fix
import higherkindness.droste.GAlgebraM

sealed abstract class Json[A]
case class JString[A](s: String) extends Json[A]
case class JInt[A](i: Int) extends Json[A]
case class JNull[A]() extends Json[A]
case class JArray[A](values: Array[Json[A]]) extends Json[A]
case class JObject[A](value: Map[String, Json[A]]) extends Json[A]

object JsonSearch {
  type JsonFix = Fix[Json]

  // find a string key somewhere in a json object
  def searchAlgebra(s: String): GAlgebraM[Option, Json, Fix[Json], Fix[Json]] = new GAlgebraM({
      case JString(_) | JInt(_) | JNull() | JArray(_) => None
      case JObject(v) if v.contains(s) =>
          v.get(s).map(Fix[Json](_))
      case JObject(v) =>
          v.values.toList traverse
  })
}
