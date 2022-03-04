package io.github.jisantuc.drosteplayground

import higherkindness.droste.data.Fix
import higherkindness.droste.scheme.hyloM
import higherkindness.droste.CoalgebraM
import higherkindness.droste.AlgebraM
import cats.Traverse
import cats.Eval
import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.kernel.Eq

sealed abstract class Json[A]
case class JString[A](s: String) extends Json[A]
case class JInt[A](i: Int) extends Json[A]
case class JNull[A]() extends Json[A]
case class JList[A](values: List[A]) extends Json[A]
case class JObject[A](value: Map[String, A]) extends Json[A]

object Json {
  def jlist[A](vs: List[A]): Json[A] = JList(vs)
  def jint[A](i: Int): Json[A] = JInt[A](i)
  def jstr[A](s: String): Json[A] = JString(s)
  def jnull[A]: Json[A] = JNull[A]()

  implicit def eqJson[A: Eq]: Eq[Json[A]] = Eq.fromUniversalEquals
  implicit val traverseJson: Traverse[Json] = new Traverse[Json] {

    override def foldLeft[A, B](fa: Json[A], b: B)(f: (B, A) => B): B =
      fa match {
        // we can't get a value of type a from any of strings, ints, or nulls, so
        // all we can do to get a b is return b
        case JInt(_) | JString(_) | JNull() => b
        // with an List or a map, we can push the foldLeft down to the inner structure
        case JList(vs) => vs.foldLeft(b)(f)
        case JObject(m) =>
          m.foldLeft(b)({ case (b_, (_, v)) =>
            f(b_, v)
          })
      }

    override def foldRight[A, B](fa: Json[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] = fa match {
      // we can't get a value of type a from any of strings, ints, or nulls, so
      // all we can do to get an Eval[B] is return lb
      case JInt(_) | JString(_) | JNull() => lb
      // similar to foldLeft, we can push the foldRight down to the inner structure
      case JList(vs) => vs.foldRight(lb)(f)
      case JObject(m) =>
        m.foldRight(lb)({ case ((_, a), lb_) =>
          f(a, lb)
        })
    }

    override def traverse[G[_]: Applicative, A, B](fa: Json[A])(
        f: A => G[B]
    ): G[Json[B]] = fa match {
      case JInt(i)    => JInt[B](i).pure[G].widen
      case JString(s) => JString[B](s).pure[G].widen
      case JNull()    => JNull[B]().pure[G].widen
      case JList(vs) =>
        vs.traverse(f).map(JList(_))
      case JObject(m) =>
        (m.toList
          .traverse { case (s, a) =>
            f(a).map((s, _))
          })
          .map(pairs => JObject(Map(pairs: _*)))
    }
  }
}

object JsonSearch {
  type JsonFix = Fix[Json]

  def jsonSearch(s: String, js: JsonFix): Option[JsonFix] = {
    // has to be def, since it recurses
    def coalgebra: CoalgebraM[Option, Json, Fix[Json]] =
      CoalgebraM[Option, Json, Fix[Json]] { fixJson =>
        println(s"Calling coalgebra with json: ${fixJson}")
        fixJson match {
          case Fix(JString(_)) | Fix(JInt(_)) | Fix(JNull()) =>
            Option.empty[Json[Fix[Json]]]
          case Fix(JObject(m)) if !m.contains(s) =>
            println(s"Keys: ${m.keySet}; target: $s")
            m.values.foldLeft(Option.empty[Json[Fix[Json]]])({
              case (acc, json) =>
                acc orElse coalgebra(json)
            })
          case Fix(JObject(m)) =>
            println(s"Keys contained: ${m.keySet}; target: $s")
            m.get(s).map(Fix.un(_))
        }
      }

    val algebra: AlgebraM[Option, Json, Fix[Json]] =
      AlgebraM[Option, Json, Fix[Json]] { json =>
        println(s"Calling algebra with json $json")
        json match {
          case JObject(m) => m.get(s)
          case jl @ JList(_) =>
            println(s"JL: $jl")
            Some(Fix(jl))
          case v =>
            println(s"V: $v")
            Some(Fix(v))
        }

      }

    val search = hyloM(algebra, coalgebra)

    search(js)
  }
}
