package nand2tetris
package asm

type Program = List[Instruction]

// an assembly program has lines
// lines can be comments. should check if empty
// lines can be labels
// any line that is not an instruction doesn't progress the address

// case class Line(
//   label: Option[Label],
//   inst: Option[Instruction],
//   comment: Option[String]
// )
// TODO: Extend spec to allow label/inst/comment on the same line
type Comment = String
type Line = Label | Instruction | Comment

// @main def assemble(program: Program): List[ml.Instruction] =
//   translate(program, symbolTable(program))

// def symbolTable(program: Program): Map[Symbol, Binary] =
//   ???

// def translate(program: Program, table: Map[Symbol, Binary]): List[ml.Instruction] = ???

sealed trait Instruction

type Constant = Int // 0x0 - 0x7FFF // TODO: Validate
// Symbols can refer to ROM or RAM and deref to Constant.
// Hardcoded constants stay unchanged,
// and should probably not reference memory
// Do we always know which? Can we tag those values?

object Constant:
  def parse(n: Int): Constant =
    require((0 to 0x7FFF).contains(n), "Constants are unsigned 15 bit")
    n

sealed trait Symbol
sealed trait PredefSymbol extends Symbol:
  def value: Constant

enum VirtualRegister extends PredefSymbol:
  case R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15
  override def value = this.ordinal

// case object SP extends PredefSymbol { def value = 0 }

enum HardcodedSymbol extends PredefSymbol:
  case SP, LCL, ARG, THIS, THAT, SCREEN, KBD
  override def value = this match
    case SP => 0
    case LCL => 1
    case ARG => 2
    case THIS => 3
    case THAT => 4
    case SCREEN => 0x4000
    case KBD => 0x6000

case class Label(name: String) extends Symbol
case class Var(name: String) extends Symbol

object Symbol:
  private val regIndices = (0 to 15).map(_.toString)
  def parse(name: String): Symbol = name match
    case r@s"R$index" if regIndices.contains(index) =>
      VirtualRegister.fromOrdinal(index.toInt)
    case "SP" => HardcodedSymbol.SP
    case "LCL" => HardcodedSymbol.LCL
    case "ARG" => HardcodedSymbol.ARG
    case "THIS" => HardcodedSymbol.THIS
    case "THAT" => HardcodedSymbol.THAT
    case "SCREEN" => HardcodedSymbol.SCREEN
    case "KBD" => HardcodedSymbol.KBD
    case s"$name" if name.forall(_.isUpper) => Label(name)
    case s"$name" if name.forall(_.isLower) => Var(name)
    case s"$name" => throw new Exception(s"symbol isn't label or var? $name")

case class AInst(xxx: Constant | Symbol) extends Instruction

enum Reg:
  case A, D, M

object Reg:
  def parse(raw: String): Reg = Reg.valueOf(raw)

  val noDest: Set[Reg] = Set.empty
  def parseDest(raw: String): Set[Reg] =
    assert(raw.length <= 3)
    assert(raw.nonEmpty)
    val regs = raw.map(c => Reg.parse(c.toString))
    // assert(regs.unique.size == raw.size)
    regs.toSet

enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

object Jump:
  def parse(raw: String): Jump =
    require(raw != "Null")
    Jump.valueOf(raw)

sealed trait Comp
// comp left side is 0 or D
// comp right side is 0 or A or M
// TODO: A+M, etc is not possible
// either a validation step, or not representable
// D=D+A
// Note can never add a reg to itself
case class Add(left: Reg, right: Reg) extends Comp
case class And(left: Reg, right: Reg) extends Comp
case class Sub(left: Reg, right: Reg) extends Comp
case class Or(left: Reg, right: Reg) extends Comp
case class Inc(arg: Reg) extends Comp
case class Dec(arg: Reg) extends Comp
case class Neg(arg: Reg) extends Comp // Reg: A,D,M
case class Not(arg: Reg) extends Comp // A,D,M
case class Noop(arg: Reg | Const)
sealed trait Const
object Const:
  case object Zero extends Const
  case object One extends Const
  case object NegOne extends Const

object Comp:
  def parse(raw: String) = raw match
    case "0" => Const.Zero
    case "1" => Const.One
    case "-1" => Const.NegOne
    case s"-$arg" => Neg(Reg.parse(arg))
    case s"!$arg" => Not(Reg.parse(arg))
    case s"$arg+1" => Inc(Reg.parse(arg))
    case s"$arg-1" => Dec(Reg.parse(arg))
    // TODO: left is always D?
    // TODO: Always A or M?
    case s"$lhs+$rhs" => Add(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs-$rhs" => Sub(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs&$rhs" => And(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs|$rhs" => Or(Reg.parse(lhs), Reg.parse(rhs))
    case reg => Noop(Reg.parse(reg))

// GOAL
// Assembler.compile(
//   List(
//     Dest.D = Reg.A + Reg.D,
//     Dest.D = Reg.A + 1
//   )
// ).toBinary

case class CInst(
  dest: Set[Reg],
  comp: Comp,
  jump: Jump // if null, don't show in string
) extends Instruction

object CInst:
  def parse(raw: String): CInst =
    def parseDest(raw: String): (Set[Reg], String) =
      if raw.contains("=") then
        val (dest, rem) = raw.splitAt(raw.indexOf("="))
        Reg.parseDest(dest) -> rem.drop(1)
      else
        Reg.noDest -> raw
    ???

    def parseJump(raw: String): (Jump, String) =
      if raw.contains(";") then
        val (rem, jump) = raw.splitAt(raw.indexOf(";"))
        Jump.parse(jump.drop(1)) -> rem
      else
        Jump.Null -> raw

    import util.chaining.*

    parseDest(raw).pipe: (dest, rem) =>
      parseJump(rem).pipe: (jump, rem) =>
        CInst(
          dest = dest,
          comp = Comp.parse(rem),
          jump = jump
        )

    // def parseComp(raw: String): (Comp, String)
    // val dest =


//
