package nand2tetris.asm

sealed trait AssemblyInstruction

type Constant = Int // 0x0 - 0x7FFF // TODO: Validate

sealed trait Symbol
sealed trait PredefSymbol extends Symbol
enum VirtualRegister:
  case R00, R01, R02, R03, R04, R05, R06, R07, R08, R09, R10, R11, R12, R13, R14, R15

case object SP extends PredefSymbol
case object LCL extends PredefSymbol
case object ARG extends PredefSymbol
case object THIS extends PredefSymbol
case object THAT extends PredefSymbol
// SCREEN 0x4000
// KBD 0x6000
case class Label(name: String) extends Symbol
case class Var(name: String) extends Symbol

case class AInst(xxx: Constant | Symbol) extends AssemblyInstruction

enum Dest:
  A, D, M

enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

// left side is 0 or D
// right side is 0 or A or M

type Reg = String
type Arg = String

sealed trait Comp
// TODO: A+M, etc is not possible
// either a validation step, or not representable
// D=D+A
case class Add(left: Reg, right: Reg | Const) extends Comp
case class And(left: Reg, right: Reg | Const) extends Comp
case class Sub(left: Reg, right: Reg | Const) extends Comp
case class Or(left: Reg, right: Reg | Const) extends Comp
case class Neg(arg: Reg | Const) extends Comp // A,D,M
case class Not(arg: Reg) extends Comp // A,D,M
sealed trait Const extends Comp
object Const:
  case object Zero extends Const
  case object One extends Const


// GOAL
// Assembler.compile(
//   List(
//     Dest.D = Reg.A + Reg.D,
//     Dest.D = Reg.A + 1
//   )
// ).toBinary

case class CInst(
  dest: Set[Dest],
  comp: Comp,
  jump: Jump // if null, don't show in string
)

//
