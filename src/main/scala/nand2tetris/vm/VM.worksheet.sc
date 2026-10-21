import nand2tetris.*
import nand2tetris.asm.toML

// push constant 7
// push constant 8
// add

val vmCommands = List(
  vm.Push(vm.Segment.Constant, cpu.U15(7)),
  vm.Push(vm.Segment.Constant, cpu.U15(8)),
  vm.ArithmeticLogical.Add
)

val asmEndLoop = asm.Program.parse(
  """|(END)
     |  @END
     |  0;JMP""".stripMargin
)

val translation = vmCommands.foldLeft(
  (Nil: asm.Program) -> vm.VMContext("Foo.vm", 0)
){
  case ((lines, ctx), command) =>
    val (moreLines, nextCtx) = command.translate(ctx)
    (lines :++ moreLines) -> nextCtx
}

val asmProgram: asm.Program = translation._1 ++ asmEndLoop

val mlProgram: ml.Program = toML(asmProgram)

asmProgram foreach println
mlProgram foreach println

val init = cpu.State
  .initRom(mlProgram.toVector)
  .setSp(cpu.Word(0x100))

val states = LazyList.iterate(init)(_.step)


states(0).sp
states(4).ram(cpu.U15(0x100))
states(5).ram(cpu.U15(0x100))
states(7).sp
states(11).ram(cpu.U15(0x101))
states(12).ram(cpu.U15(0x101))
states(14).sp
states(15).a
states(15).m
states(15).sp // 257
states(19).sp // 257

states(100).sp
states(100).ram(cpu.U15(256))


//
