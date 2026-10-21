package nand2tetris
package vm

class VmTranslateSuite extends munit.FunSuite:
  def testBinop(x: Int, y: Int, op: ArithmeticLogical, expected: Int): Unit =
    test(s"$x $y $op = $expected"):
      val vmCommands = List(
        vm.Push(vm.Segment.Constant, cpu.U15(x)),
        vm.Push(vm.Segment.Constant, cpu.U15(y)),
        op
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
      val mlProgram: ml.Program = asm.toML(asmProgram)

      val init = cpu.State
        .initRom(mlProgram.toVector)
        .setSp(cpu.Word(0x100))

      val states = LazyList.iterate(init)(_.step)
      val endState = states(100)

      assertEquals(endState.ram(cpu.U15(0x100)), cpu.Word(expected))

  import ArithmeticLogical.*
  testBinop(3, 4, Add, 7)
  testBinop(7, 4, Sub, 3)
  testBinop(5, 2, And, 0)
  testBinop(5, 2, Or, 7)

  // TODO push constant -1 <- is this valid?
  // because arg is an index into segment
  // testBinop(-1, -1, And, -1)
  // testBinop(-1, 0, And, 0)
  testBinop(0, 0, Or, 0)
  // testBinop(-1, 0, Or, -1)

  testBinop(5, 2, Eq, 0)
  testBinop(5, 2, Gt, -1)
  testBinop(5, 2, Lt, 0)

  testBinop(5, 5, Eq, -1)
  testBinop(5, 5, Gt, 0)
  testBinop(5, 5, Lt, 0)

  testBinop(2, 5, Eq, 0)
  testBinop(2, 5, Gt, 0)
  testBinop(2, 5, Lt, -1)
