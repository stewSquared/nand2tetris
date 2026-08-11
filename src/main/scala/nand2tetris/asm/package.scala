package nand2tetris
package asm

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
object Comment:
  def apply(s: String): Comment = s

type Line = Label | Inst | Comment
type Program = List[Line]
object Program:
  def parse(raw: String): Program =
    raw.linesIterator.toList.map(Line.parse)

object Line:
  def parse(raw: String): Line = raw.strip() match
    case s"//$comment" => Comment(comment)
    case s"($label)" => Label(label)
    case rawInst => Inst.parse(rawInst)

// @main def assemble(program: Program): List[ml.Instruction] =
//   translate(program, symbolTable(program))

type Address = Int // 0x0000 - 0x7FFF // TODO: verify range

def symbolTable(program: Program): Map[Symbol, Address] =
  case class State(instCount: Int, varCount: Int, table: Map[Symbol, Address]):
    def countInst: State = copy(instCount = instCount + 1)
    def countVar: State = copy(varCount = varCount + 1)
    def assoc(sym: Symbol, adr: Address): State = copy(table = table.updated(sym, adr))

  val state = program.foldLeft(State(0, 0, Map.empty)):
    case (state, comment: Comment) => state
    case (state, label: Label) => state.assoc(label, state.instCount)
    case (state, AInst(sym: Var)) if !state.table.contains(sym) =>
      state
        .countInst
        .countVar
        .assoc(sym, 16 + state.varCount)
    case (state, _: Inst) => state.countInst

  state.table

def deref(program: Program, table: Map[Symbol, Address]): Program =
  program.map:
    case AInst(sym: PredefSymbol) => AInst(sym.value)
    case AInst(sym: Symbol) => AInst(table(sym): Constant)
    case line => line

def toML(program: Program): ml.Program =
  val table = symbolTable(program)
  val mlc = ml.Comp.default
  deref(program, table).collect:
    case AInst(sym: Symbol) => throw new Exception("program not dereferenced") // TODO make this not compile
    case AInst(adr: Constant) => ml.AInst(adr)
    case CInst(dest, comp, jump) =>
      val mlComp = comp match
        case Noop(arg) => arg match
          case Reg.D        => mlc.x.and.yNegOne
          case Reg.A        => mlc.xNegOne.and.y
          case Reg.M        => mlc.xNegOne.and.y.fromMem
          case Const.One    => mlc.xNegOne.add.yNegOne.negate
          case Const.Zero   => mlc.zeroX.add.zeroY
          case Const.NegOne => mlc.xNegOne.add.zeroY
        // TODO maybe normalize operand order wrt D
        case Add(_, Reg.A) => mlc.x.add.y
        case Add(_, Reg.M) => mlc.x.add.y.fromMem
        case And(_, Reg.A) => mlc.x.and.y
        case And(_, Reg.M) => mlc.x.and.y.fromMem
        case Or(_, Reg.A) => mlc.negX.and.negY.negate
        case Or(_, Reg.M) => mlc.negX.and.negY.negate.fromMem
        case Sub(Reg.D, Reg.A) => mlc.negX.add.y.negate
        case Sub(Reg.A, Reg.D) => mlc.negY.add.x.negate
        case Sub(Reg.D, Reg.M) => mlc.negX.add.y.negate.fromMem
        case Sub(Reg.M, Reg.D) => mlc.negY.add.x.negate.fromMem
        case Not(Reg.D) => mlc.negX.and.yNegOne.negate
        case Not(Reg.A) => mlc.negY.and.xNegOne.negate
        case Not(Reg.M) => mlc.negY.and.xNegOne.negate.fromMem
        case Neg(Reg.D) => mlc.negX.add.yNegOne.negate
        case Neg(Reg.A) => mlc.negY.add.xNegOne.negate
        case Neg(Reg.M) => mlc.negY.add.xNegOne.negate.fromMem
        case Inc(Reg.D) => mlc.negX.add.yNegOne.negate
        case Inc(Reg.A) => mlc.negY.add.xNegOne.negate
        case Inc(Reg.M) => mlc.negY.add.xNegOne.negate.fromMem
        case Dec(Reg.D) => mlc.x.add.yNegOne
        case Dec(Reg.A) => mlc.y.add.xNegOne
        case Dec(Reg.M) => mlc.y.add.xNegOne.fromMem


      ml.CInst(
        comp = mlComp,
        dest = ml.Dest(
          a = dest.contains(Reg.A),
          d = dest.contains(Reg.D),
          m = dest.contains(Reg.M)
        ),
        jump = ml.Jump.fromOrdinal(jump.ordinal), // TODO: Hacky/redundant?
      )

// def translate(program: Program, table: Map[Symbol, Binary]): List[ml.Instruction] = ???



sealed trait Inst

object Inst:
  def parse(raw: String): Inst =
    if raw.startsWith("@") then AInst.parse(raw)
    else CInst.parse(raw)

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

case class AInst(xxx: Constant | Symbol) extends Inst

object AInst:
  def parse(raw: String): AInst = raw match
    case s"@$xxx" =>
      AInst:
        xxx.toIntOption.map: n =>
          Constant.parse(n)
        .getOrElse(Symbol.parse(xxx))
    case _ => throw new Exception("A-inst must start with @")

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

sealed trait Comp:
// comp left side is 0 or D
// comp right side is 0 or A or M
// TODO: A+M, etc is not possible
// either a validation step, or not representable
// D=D+A
// Note can never add a reg to itself
  def fromMem: Boolean = this match
    case op: BinOp => op.right == Reg.M
    case op: UnOp => op.arg == Reg.M
    case Noop(Reg.M) => true
    case _ => false

sealed trait BinOp extends Comp:
  def left: Reg
  def right: Reg

sealed trait UnOp extends Comp:
  def arg: Reg

// TODO: in Add and AND, left is always D (right?)
case class Add(left: Reg, right: Reg) extends BinOp
case class And(left: Reg, right: Reg) extends BinOp
case class Sub(left: Reg, right: Reg) extends BinOp
case class Or(left: Reg, right: Reg) extends BinOp
case class Inc(arg: Reg) extends UnOp
case class Dec(arg: Reg) extends UnOp
case class Neg(arg: Reg) extends UnOp // Reg: A,D,M
case class Not(arg: Reg) extends UnOp // A,D,M
case class Noop(arg: Reg | Const) extends Comp
sealed trait Const
object Const:
  case object Zero extends Const
  case object One extends Const
  case object NegOne extends Const

object Comp:
  def parse(raw: String): Comp = raw match
    case "0" => Noop(Const.Zero)
    case "1" => Noop(Const.One)
    case "-1" => Noop(Const.NegOne)
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
) extends Inst

object CInst:
  def parse(raw: String): CInst =
    def parseDest(raw: String): (Set[Reg], String) =
      if raw.contains("=") then
        val (dest, rem) = raw.splitAt(raw.indexOf("="))
        Reg.parseDest(dest) -> rem.drop(1)
      else
        Reg.noDest -> raw

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


//
