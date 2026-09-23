package nand2tetris
package asm

// Symbols can refer to ROM or RAM and deref to Address.
type Address = cpu.U15
object Address:
  def apply(n: Int): Address = cpu.U15.apply(n)

type RomAddress = Address
object RomAddress:
  def apply(n: Int): RomAddress = cpu.U15.apply(n)

type RamAddress = Address
object RamAddress:
  def apply(n: Int): RamAddress = cpu.U15.apply(n)

type Constant = cpu.U15
object Constant:
  def parse(n: Int): Constant = cpu.U15.apply(n)

type SymbolTable = Map[Symbol, Address]

def symbolTable(program: Program): SymbolTable =
  case class State(instCount: Int, varCount: Int, table: SymbolTable):
    def countInst: State = copy(instCount = instCount + 1)
    def countVar: State = copy(varCount = varCount + 1)
    def assoc(sym: Symbol, adr: Address): State = copy(table = table.updated(sym, adr))

  val labels: SymbolTable = program.collect:
    case label: Label => label.sym -> RomAddress(0)
  .toMap

  val state = program.foldLeft(State(0, 0, labels)):
    case (state, comment: Comment) => state
    case (state, Label(sym)) => state.assoc(sym, RomAddress(state.instCount))
    case (state, AInst(varSym: UserSymbol)) if !state.table.contains(varSym) =>
      state
        .countInst
        .countVar
        .assoc(varSym, RamAddress(16 + state.varCount))
    case (state, _: Inst) => state.countInst

  state.table

// TODO: DerefProgram should be it's own type
def deref(program: Program, table: SymbolTable): List[Inst] =
  program.flatMap:
    case AInst(sym: PredefSymbol) => Some(AInst(sym.value))
    case AInst(sym: Symbol) => Some(AInst(table(sym): Address))
    case inst: Inst => Some(inst)
    case line => None

def toML(program: Program): ml.Program =
  val table = symbolTable(program)
  deref(program, table).collect(_.toML)
