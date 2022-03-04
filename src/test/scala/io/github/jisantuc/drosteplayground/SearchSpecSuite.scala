package io.github.jisantuc.drosteplayground

import higherkindness.droste.scheme
import munit.FunSuite
import higherkindness.droste.data.Fix
import cats.laws.TraverseLaws
import cats.Traverse
import cats.laws.discipline.TraverseTests
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class SearchSpecSuite extends FunSuite with munit.DisciplineSuite {

  private def genJson[A: Arbitrary]: Gen[Json[A]] = Gen.oneOf(
    Gen.const("string").map(Json.jstr[A](_)),
    Gen.const(3).map(Json.jint[A](_)),
    Gen.const(Json.jnull[A]),
    Gen
      .listOfN(3, Arbitrary.arbitrary[A])
      .map(jsonValues => Json.jlist(jsonValues)),
    Gen
      .mapOf(for {
        key <- Gen.alphaLowerStr
        value <- Arbitrary.arbitrary[A]
      } yield (key, value))
      .map(JObject(_))
  )
  implicit def arbJson[A: Arbitrary]: Arbitrary[Json[A]] = Arbitrary {
    genJson[A]
  }

  checkAll(
    "traverse laws",
    TraverseTests[Json].traverse[Int, String, Int, Int, Option, Option]
  )

  test("find json keys when present in simple objects") {
    val jint: Fix[Json] = Fix(JInt(1))
    val testJson: Fix[Json] = Fix(
      JObject(
        Map(
          "a" -> jint
        )
      )
    )

    assertEquals(
      JsonSearch.jsonSearch("a", testJson),
      Some(jint)
    )
  }

  test("find json keys when present in complex objects") {
    val jstring: Fix[Json] = Fix(JString("hello"))
    val testJson: Fix[Json] = Fix(
      JObject(
        Map(
          "b" -> Fix(
            JObject(
              Map(
                "a" -> jstring,
                "c" -> Fix(
                  JObject(
                    Map("d" -> Fix(JNull()))
                  )
                )
              )
            )
          )
        )
      )
    )

    assertEquals(
      JsonSearch.jsonSearch("a", testJson),
      Some(jstring)
    )

  }

  test("find json keys when present when they contains a list") {
    val jList: Fix[Json] =
      Fix(JList(List(Fix(JInt(1)), Fix(JString("3")), Fix(JNull()))))

    val testJson: Fix[Json] = Fix(
      JObject(
        Map(
          "b" -> Fix(
            JObject(
              Map(
                "a" -> jList
              )
            )
          )
        )
      )
    )

    assertEquals(JsonSearch.jsonSearch("a", testJson), Some(jList))
  }

  test("find json keys when present when they contain an object") {
    val jObj: Fix[Json] = Fix(JObject(Map("a" -> Fix(JInt(1)))))

    val testJson: Fix[Json] = Fix(
      JObject(
        Map("b" -> jObj)
      )
    )

    assertEquals(
      JsonSearch.jsonSearch("b", testJson),
      Some(jObj)
    )
  }

  test("not find missing keys") {
    val testJson: Fix[Json] = Fix(JNull())
    assertEquals(
      JsonSearch.jsonSearch("a", testJson),
      Option.empty[Fix[Json]]
    )
  }

}
