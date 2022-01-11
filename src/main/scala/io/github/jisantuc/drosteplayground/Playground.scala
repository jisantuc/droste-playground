package io.github.jisantuc.drosteplayground

import higherkindness.droste.Algebra
import cats.Functor
import higherkindness.droste.data.Fix

case class HPosition(value: Int) extends AnyVal
case class VPosition(value: Int) extends AnyVal

object Playground {
  // part 1 -- no explicit recursion, something else will handle the structure
  // e.g., we'll have a list of instructions
  sealed abstract class Instruction
  final case class HAdjust(hDelta: Int) extends Instruction
  final case class VAdjust(vDelta: Int) extends Instruction
  final case class End() extends Instruction

  // our fold in that case processes the instructions in order
  def eval(instructions: List[Instruction]) =
    instructions.foldLeft((HPosition(0), VPosition(0)))(???)

  // part 2 -- explicit recursion; each will take a next param that is also an instruction
  // except End which just says that it's over
  sealed abstract class InstructionR
  final case class HAdjustR(hDelta: Int, next: InstructionR)
      extends InstructionR
  final case class VAdjustR(vDelta: Int, next: InstructionR)
      extends InstructionR
  final case class EndR() extends InstructionR

  def evalR(
      instr: InstructionR
  ): ((HPosition, VPosition)) => (HPosition, VPosition) = {
    (position: (HPosition, VPosition)) =>
      instr match {
        case HAdjustR(delta, next) =>
          val (h, v) = position
          evalR(next)(HPosition(h.value + delta), v)
        case VAdjustR(delta, next) =>
          val (h, v) = position
          evalR(next)(h, VPosition(v.value + delta))
        case EndR() => position
      }
  }

  // part 3 -- factor out the recursion
  sealed abstract class InstructionF[A]
  final case class HAdjustF[A](hDelta: Int, next: A) extends InstructionF[A]
  final case class VAdjustF[A](vDelta: Int, next: A) extends InstructionF[A]
  final case class EndF[A]() extends InstructionF[A]

  // HAdjustF[VAdjustF[EndF[Nothing]]] _gross_
  // type of expressio now depends on the shape of the expression. that's maybe bad!
  val exampleExpression = HAdjustF(3, VAdjustF(4, EndF()))

  // much nicer type 😎
  // cost: lots of Fix(...)es
  val exampleExpressionFix: Fix[InstructionF] = Fix(
    HAdjustF(3, Fix(VAdjustF(4, Fix(EndF()))))
  )

  implicit val instructionFFunctor: Functor[InstructionF] =
    new Functor[InstructionF] {
      def map[A, B](fa: InstructionF[A])(f: A => B): InstructionF[B] =
        fa match {
          case HAdjustF(h, n) => HAdjustF(h, f(n))
          case VAdjustF(v, n) => VAdjustF(v, f(n))
          case EndF()         => EndF()
        }
    }

  type Position = (HPosition, VPosition)

  type Adjust = Position => Position

  val algAdjust: Algebra[InstructionF, Adjust] = Algebra {
    case HAdjustF(h, n) =>
      (pos: Position) => n((HPosition(pos._1.value + h), pos._2))
    case VAdjustF(v, n) =>
      (pos: Position) => n((pos._1, VPosition(pos._2.value + v)))
    case EndF() => identity[Position](_)
  }

}
