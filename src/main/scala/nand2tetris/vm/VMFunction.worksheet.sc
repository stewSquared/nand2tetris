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

val labels = asmProgram.collect:
  case label: asm.Label => label

labels foreach println

asm.symbolTable(asmProgram).underlying foreach println

val endAddress =
  asm.symbolTable(asmProgram)(asm.UserSymbol("END"))

val rom = asm.toML(asmProgram).toVector

// INIT
// set SP to 256 0x100
// Main.main

val init = cpu.State
  .initRom(rom)
  .setSp(cpu.Word(0x100))

val states = LazyList.iterate(init)(_.step)

states(197).pc
states(197).ram.underlying(14)

states(200)
states(200).pc
states(300).pc

states(110)
states(110).pc
states(110).sp

states(110).ram.underlying.slice(256,269)
  .foreach(println)


states(146)

states(157)

states(158)

states(145).ram(cpu.U15(14))
states(146).pc

// states(147)

// states(1000).pc
// states(1001).pc

// states(200)
// states(200).pc

// asmInstructions(66)
// rom(66)


// states(10000)
// states(10000).pc

def printProgram(): Unit =
  val symbolTable = asm.symbolTable(asmProgram)
  var count = 0
  asmProgram.foreach:
    // case line: asm.AInst.Ref =>
    //   println(s"$count: $line (${symbolTable(line.sym)})")
    //   count += 1
    case line: asm.Inst =>
      println(s"$count: $line")
      count += 1
    case line =>
      println(s"$count: $line")

printProgram()

// asmInstructions.zipWithIndex.foreach: (asm, i) =>
//   println(s"$i: $asm ${rom(i)}")

// asmProgram foreach println

states.zipWithIndex.take(157).foreach: (state, clock) =>
  val pointers = state.ram.underlying.slice(0, 5)
  val temp = state.ram.underlying.slice(13, 15)
  val stack = state.ram.underlying.slice(256, state.sp.toInt)
  println(s"$clock: ${pointers.mkString(" ")} | ${temp.mkString(" ")} | ${stack.mkString(" ")}")

states(157).a

states.zipWithIndex.take(350).foreach: (state, clock) =>
  println(s"$clock: pc=${state.pc} sp=${state.sp} top=${state.ram(cpu.U15(state.sp.toInt - 1))}")


states(4).sp

states(10).sp
states(10).ram(cpu.U15(256))
states(10).pc

states(16).sp
states(16).ram(cpu.U15(257))

states(34).sp
states(40).arg

states(44).pc
states(45).a
states(46).pc
states(46).sp

states(53).pc
states(53).sp
states(53).a // return address


states(48).pc
states(48).sp
states(49).a
states(49).m
states(49).sp


states(50).a

states(48).a

states(50).pc
states(51).pc
states(51).a


// states(51).m
states(51).d
states(50).d


rom(36)
rom(37)
rom(38)
rom(39)
rom(40)
rom(41)
rom(42)
rom(43)
rom(44)
rom(45)
rom(46)
rom(47)
rom(48)
rom(49)
rom(50)
rom(51)
rom(52)


// states(100)



//
