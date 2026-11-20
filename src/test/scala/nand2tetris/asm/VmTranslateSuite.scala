package nand2tetris
package vm

class VmTranslateSuite extends munit.FunSuite:
  def testBinop(x: Int, y: Int, op: ArithmeticLogical, expected: Int): Unit =
    test(s"push constant $x; push constant $y; $op = $expected"):
      val vmCommands =
        s"""|push constant $x
            |push constant $y
            |$op""".stripMargin.linesIterator.map(VMCommand.parse)

      val asmEndLoop = asm.Program.parse(
        """|(END)
           |  @END
           |  0;JMP""".stripMargin
      )

      // TODO: use `sequence`?
      val translation = vmCommands.foldLeft(Translation.pure[asm.Program](Nil)):
        case (txn, command) => for
          lines <- txn
          moreLines <- command.translate
        yield lines ++ moreLines

      val asmProgram = translation.map(_ ++ asmEndLoop)

      val initContext = vm.VMContext("Foo.vm", 0, "foo")

      val mlProgram = asm.toML(asmProgram.run(initContext)._1)

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
  testBinop(7, 5, And, 5)
  testBinop(2, 5, And, 0)
  testBinop(0, 5, Or, 5)
  testBinop(5, 2, Or, 7)

  testBinop(5, 2, Eq, 0)
  testBinop(5, 2, Gt, -1)
  testBinop(5, 2, Lt, 0)

  testBinop(5, 5, Eq, -1)
  testBinop(5, 5, Gt, 0)
  testBinop(5, 5, Lt, 0)

  testBinop(2, 5, Eq, 0)
  testBinop(2, 5, Gt, 0)
  testBinop(2, 5, Lt, -1)

  test("test that goto works"):
    val vmCommands = List(
      vm.Push(Segment.Constant, cpu.U15(3)),
      vm.Push(Segment.Constant, cpu.U15(4)),
      vm.GoTo("FOO"),
      vm.ArithmeticLogical.Add,
      vm.Label("FOO"),
    )

    // TODO: use `sequence`?
    val translation = vmCommands.foldLeft(Translation.pure[asm.Program](Nil)):
      case (txn, command) => for
        lines <- txn
        moreLines <- command.translate
      yield lines ++ moreLines

    val asmEndLoop = asm.Program.parse(
      """|(END)
         |  @END
         |  0;JMP""".stripMargin
    )

    val asmProgram = translation.map(_ ++ asmEndLoop)

    val initContext = vm.VMContext("Foo.vm", 0, "foo")

    val mlProgram = asm.toML(asmProgram.run(initContext)._1)

    val init = cpu.State
      .initRom(mlProgram.toVector)
      .setSp(cpu.Word(0x100))

    val states = LazyList.iterate(init)(_.step)
    val endState = states(100)

    assertEquals(endState.ram(cpu.U15(0x100)), cpu.Word(3))
    assertEquals(endState.ram(cpu.U15(0x101)), cpu.Word(4))
    assertEquals(endState.sp, cpu.Word(0x102))

  test("test that if-goto works"):
    val vmCommands =
      s"""|push constant 3
          |push constant 0
          |if-goto FOO
          |push constant 4
          |if-goto FOO
          |add
          |label foo""".stripMargin.linesIterator.map(vm.VMCommand.parse)

    // TODO: use `sequence`?
    val translation = vmCommands.foldLeft(Translation.pure[asm.Program](Nil)):
      case (txn, command) => for
        lines <- txn
        moreLines <- command.translate
      yield lines ++ moreLines

    val asmEndLoop = asm.Program.parse(
      """|(END)
         |  @END
         |  0;JMP""".stripMargin
    )

    val asmProgram = translation.map(_ ++ asmEndLoop)

    val initContext = vm.VMContext("Foo.vm", 0, "foo")

    val mlProgram = asm.toML(asmProgram.run(initContext)._1)

    val init = cpu.State
      .initRom(mlProgram.toVector)
      .setSp(cpu.Word(0x100))

    val states = LazyList.iterate(init)(_.step)
    val endState = states(100)

    assertEquals(endState.ram(cpu.U15(0x100)), cpu.Word(3))
    assertEquals(endState.ram(cpu.U15(0x101)), cpu.Word(4))
    assertEquals(endState.sp, cpu.Word(0x101))
