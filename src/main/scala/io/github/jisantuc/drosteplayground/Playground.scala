package io.github.jisantuc.drosteplayground

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
    final case class HAdjustR(hDelta: Int, next: InstructionR) extends InstructionR
    final case class VAdjustR(vDelta: Int, next: InstructionR) extends InstructionR
    final case class EndR() extends InstructionR

    def evalR(instr: InstructionR): ((HPosition, VPosition)) => (HPosition, VPosition) = { (position: (HPosition, VPosition)) =>
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
}