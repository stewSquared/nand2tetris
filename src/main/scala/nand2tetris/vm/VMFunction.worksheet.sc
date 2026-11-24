import nand2tetris.*
import nand2tetris.asm.symbolTable

s"""|int double(int x) {
    |  var d = x + x;
    |  return d;
    |}""".stripMargin

val vmCommands: List[vm.VMCommand] =
  val vmCode =
    // s"""|push constant 1
    //     |push constant 1""".stripMargin
    s"""|function Main.main 0
        |  push constant 1
        |  call Main.identity 1
        |  return
        |function Main.identity 0
        |  push argument 0
        |  push constant 1
        |  add
        |  return""".stripMargin

    // s"""|push constant 3
    //     |call identity 1
    //     |function identity 0
    //     |  push argument 0
    //     |  return""".stripMargin

    // s"""|function double 1
    //     |  push argument 0
    //     |  push argument 0
    //     |  pop local 0
    //     |  return
    //     |push constant 3
    //     |call double 1
    //     |call double 1""".stripMargin
  vmCode.linesIterator.map(vm.VMCommand.parse).toList

def translate(vmCommands: List[vm.VMCommand]): asm.Program =
  val bootstrap: vm.Translation[asm.Program] =
    val setSP = asm.Program.parse:
      """|@256
         |D=A
         |@SP
         |// M=D
         |// @LCL
         |M=D""".stripMargin
    val endLoop = asm.Program.parse:
      """|  @END
         |(END)
         |  0;JMP""".stripMargin
    vm.Call("Main.main", 0).translate.map: callMain =>
      setSP ++ callMain ++ endLoop

  val translation = vmCommands.foldLeft(bootstrap):
    case (txn, command) => for
      lines <- txn
      moreLines <- command.translate
      comment = asm.Comment(s"translating $command")
    yield lines ++ (comment +: moreLines)

  val initContext = vm.VMContext("Main", 0, "$NILL")

  translation.run(initContext)._1

val asmProgram = translate(vmCommands)

val asmInstructions = asmProgram.collect:
  case inst: asm.Inst => inst

val symbolTable =  asm.symbolTable(asmProgram)

symbolTable.underlying foreach println

val endAddress = symbolTable(asm.UserSymbol("END"))

val rom = asm.toML(asmProgram).toVector

val init = cpu.State
  .initRom(rom)
  .setSp(cpu.Word(0x100))

val states = LazyList.iterate(init)(_.step)

def printProgram(): Unit =
  var count = 0
  asmProgram.foreach:
    case line: asm.Inst =>
      println(s"$count: $line")
      count += 1
    case line =>
      println(s"$count: $line")

printProgram()

states.zipWithIndex.take(250).foreach: (state, clock) =>
  val pointers = state.ram.underlying.slice(0, 5)
  val temp = state.ram.underlying.slice(13, 15)
  val stack = state.ram.underlying.slice(256, state.sp.toInt)
  println(s"$clock:${state.pc} ${pointers.mkString(" ")} | ${temp.mkString(" ")} | ${stack.mkString(" ")}")


//
