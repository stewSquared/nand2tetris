package nand2tetris.asm

import nand2tetris.ml.{ AInst => MlAInst }

case class SymbolTable(underlying: Map[Symbol, Address]):
  def apply(sym: Symbol): Address = underlying(sym)

  def resolve(symbol: Symbol): Address = symbol match
    case sym: PredefSymbol => sym.value // TODO preserve source
    case sym: UserSymbol => underlying(sym)

  def deref(ref: AInst.Ref): MlAInst = MlAInst(resolve(ref.sym))

object SymbolTable:
  val empty: SymbolTable = SymbolTable(Map.empty)

def symbolTable(program: Program): SymbolTable =
  case class State(instCount: Int, varCount: Int, table: Map[Symbol, Address]):
    def countInst: State = copy(instCount = instCount + 1)
    def countVar: State = copy(varCount = varCount + 1)
    def assoc(sym: Symbol, adr: Address): State = copy(table = table.updated(sym, adr))

  val labels: Map[Symbol, Address] =
    program.collect:
      case label: Label => label.sym -> RomAddress(0)
    .toMap

  val state = program.foldLeft(State(0, 0, labels)):
    case (state, comment: Comment) => state
    case (state, Label(sym)) => state.assoc(sym, RomAddress(state.instCount))
    case (state, AInst.Ref(varSym: UserSymbol)) if !state.table.contains(varSym) =>
      state
        .countInst
        .countVar
        .assoc(varSym, RamAddress(16 + state.varCount))
    case (state, _: Inst) => state.countInst

  SymbolTable(state.table)
