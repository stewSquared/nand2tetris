package nand2tetris
package asm

type Address = Int // 0x0000 - 0x7FFF // TODO: verify range
type Constant = Int // 0x0 - 0x7FFF // TODO: Validate
// Symbols can refer to ROM or RAM and deref to Constant.
// Hardcoded constants stay unchanged,
// and should probably not reference memory
// Do we always know which? Can we tag those values?

object Constant:
  def parse(n: Int): Constant =
    require((0 to 0x7FFF).contains(n), "Constants are unsigned 15 bit")
    n

def symbolTable(program: Program): Map[Symbol, Address] =
  case class State(instCount: Int, varCount: Int, table: Map[Symbol, Address]):
    def countInst: State = copy(instCount = instCount + 1)
    def countVar: State = copy(varCount = varCount + 1)
    def assoc(sym: Symbol, adr: Address): State = copy(table = table.updated(sym, adr))

  val labels = program.collect:
    case label: Label => label.sym -> 0

  val state = program.foldLeft(State(0, 0, labels.toMap)):
    case (state, comment: Comment) => state
    case (state, Label(sym)) => state.assoc(sym, state.instCount)
    case (state, AInst(varSym: UserSymbol)) if !state.table.contains(varSym) =>
      state
        .countInst
        .countVar
        .assoc(varSym, 16 + state.varCount)
    case (state, _: Inst) => state.countInst

  state.table

// TODO: DerefProgram should be it's own type
def deref(program: Program, table: Map[Symbol, Address]): List[Inst] =
  program.flatMap:
    case AInst(sym: PredefSymbol) => Some(AInst(sym.value))
    case AInst(sym: Symbol) => Some(AInst(table(sym): Constant))
    case inst: Inst => Some(inst)
    case line => None

// should be able to call .toML on a single inst
// any c inst, and only dereferenced a insts
// TODO: should derefrenced A inst be a different type?
// maybe make symbol table a context param

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
