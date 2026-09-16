import nand2tetris.*

val rawAsm = io.Source.fromResource("Max.asm").getLines().mkString("\n")
val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
val symbolTable = asm.symbolTable(asmProgram)
val mlProgram = nand2tetris.asm.toML(asmProgram)

val endAddr = symbolTable(asm.Symbol.parse("END"))

val init = cpu.State
  .initRom(mlProgram.toVector)
  .setR0(cpu.Word(5))
  .setR1(cpu.Word(31))

val states = LazyList.iterate(init)(_.step)
  // .takeWhile(_.pc != endAddr)

val endState = Iterator.iterate(init)(_.step)
  .dropWhile(_.pc != endAddr)
  .next()
states(4).d

states(4).d.toInt.toHexString

0xFFE6 << 16 >> 16

(5 - 31).toHexString

((5 - 31) << 16 >> 16).toHexString

endState.r2
