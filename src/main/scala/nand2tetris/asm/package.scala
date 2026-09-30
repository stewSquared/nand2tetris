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

def deref(program: Program)(using table: SymbolTable): List[Inst] =
  program.collect:
    case AInst.Ref(sym) => AInst.Val(table.resolve(sym))
    case inst: Inst => inst

def toML(program: Program): ml.Program =
  val table = symbolTable(program)
  program.collect:
    case inst: Inst => inst.toML(using table)
