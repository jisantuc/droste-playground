package io.github.jisantuc.drosteplayground

import munit.FunSuite

import Playground._

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
}
