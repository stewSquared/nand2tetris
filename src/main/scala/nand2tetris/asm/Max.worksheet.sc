import nand2tetris.*

val rawAsm = io.Source.fromResource("Max.asm").getLines().mkString("\n")
val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
val symbolTable = asm.symbolTable(asmProgram)
val mlProgram = nand2tetris.asm.toML(asmProgram)

val endAddr = cpu.Word(symbolTable(asm.Symbol.parse("END")))

val init = cpu.State
  .initRom(mlProgram.toVector)
  .setR0(cpu.Word(5))
  .setR1(cpu.Word(31))

val states = LazyList.iterate(init)(_.step)
  // .takeWhile(_.pc != endAddr)

val endState = Iterator.iterate(init)(_.step)
  .dropWhile(_.pc != endAddr)
  .next()

states(3).d
states(3).m

states(3).d

states(3).currentInstruction

states(3)
  .currentInstruction
  .asInstanceOf[ml.CInst]
  .comp


states(3)
  .currentInstruction
  .asInstanceOf[ml.CInst]
  .dest

states(3).d
states(3).m

states(4).d

states(4).currentInstruction

states(4)
states(4).d

states(4).d.toInt.toHexString
states(4).d.toInt

states(5).a.toInt
symbolTable(asm.Symbol.parse("ITSR0"))

states(5).currentInstruction.asInstanceOf[ml.CInst]

states(5).currentInstruction.asInstanceOf[ml.CInst].dest
states(5).currentInstruction.asInstanceOf[ml.CInst].comp
states(5).d
states(5).currentInstruction.asInstanceOf[ml.CInst].jump

states(5).d.toInt
states(5).d.toInt > 0

val sub = states(5).currentInstruction.asInstanceOf[ml.CInst].comp
sub.zx
sub.nx

sub.zy
sub.ny

cpu.Word(0)

~cpu.Word(0)
(~cpu.Word(0)).toInt.toHexString

states(5).d.toInt.toHexString
cpu.Word(0xFFFF)

 states(5).d & cpu.Word(0xFFFF)


states(5)
  .calcComp(states(5).currentInstruction.asInstanceOf[ml.CInst].comp)

states(5).d

sub.a



states(5).movePC(ml.Jump.JGT, cpu.Word(-26))
states(5).movePC(ml.Jump.JGT, cpu.Word(-26)).pc


states(5).step.pc


states(6).d
states(6).pc.toInt


0xFFE6 << 16 >> 16

(5 - 31).toHexString

((5 - 31) << 16 >> 16).toHexString

endState.r2

cpu.Word(0x8000)

//
