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

def toML(program: Program): ml.Program =
  val table = symbolTable(program)
  deref(program, table).collect(_.toML)
