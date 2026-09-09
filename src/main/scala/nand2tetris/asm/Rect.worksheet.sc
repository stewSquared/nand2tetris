import nand2tetris.*

val rawAsm = io.Source.fromResource("Rect.asm").getLines().mkString("\n")
val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
val symbolTable = asm.symbolTable(asmProgram)
val mlProgram = nand2tetris.asm.toML(asmProgram)

val endAddr = symbolTable(asm.Symbol.parse("END"))

val init = cpu.State
  .initRom(mlProgram.toVector)
  .setR0(4)

val endState = Iterator.iterate(init)(_.step)
  .dropWhile(_.pc != endAddr)
  .next()

endState.drawSubscreen(0, 0, 17, 8)

//
