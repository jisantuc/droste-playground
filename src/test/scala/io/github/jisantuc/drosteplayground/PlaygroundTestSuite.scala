package io.github.jisantuc.drosteplayground

import higherkindness.droste.scheme
import munit.FunSuite

import Playground._
import higherkindness.droste.Algebra
import higherkindness.droste.data.Fix

class PlaygroundTestSuite extends FunSuite {
  test("solve the sample puzzle with explicit recursion") {
    //   scalafmt has some real opinions about this, but anyway
    val samplePuzzle = HAdjustR(
      5,
      VAdjustR(5, HAdjustR(8, VAdjustR(-3, VAdjustR(8, HAdjustR(2, EndR())))))
    )

    assertEquals(
      evalR(samplePuzzle)((HPosition(0), VPosition(0))),
      (HPosition(15), VPosition(10))
    )
  }

  test("solve the sample puzzle with rec schemes") {
    // annotation is necessary here because without it we seem to hit
    // some kind of recursion limit and the compiler gets mad about the
    // EndF 😱
    val samplePuzzle: Fix[InstructionF] = Fix(
      HAdjustF(
        5,
        Fix(
          VAdjustF(
            5,
            Fix(
              HAdjustF(
                8,
                Fix(
                  VAdjustF(-3, Fix(VAdjustF(8, Fix(HAdjustF(2, Fix(EndF()))))))
                )
              )
            )
          )
        )
      )
    )

    val adjust = scheme.cata(algAdjust)

    assertEquals(
      adjust(samplePuzzle)((HPosition(0), VPosition(0))),
      (HPosition(15), VPosition(10))
    )
  }
}
