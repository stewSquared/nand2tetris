import nand2tetris.*
import nand2tetris.asm.symbolTable

s"""|int double(int x) {
    |  var d = x + x;
    |  return d;
    |}""".stripMargin

val mainSource =
  val rawCode =
    s"""|function Main.main 0
        |  push constant 3
        |  call Double.double 1
        |  call Double.double 1
        |  return""".stripMargin
  val commands = rawCode.linesIterator.map(vm.VMCommand.parse).toVector
  vm.Source("Main", commands)

val doubleSource =
  val rawCode =
    s"""|function Double.double 0
        |  push argument 0
        |  push argument 0
        |  add
        |  return""".stripMargin
  val commands = rawCode.linesIterator.map(vm.VMCommand.parse).toVector
  vm.Source("Double", commands.toVector)

mainSource.code foreach println

val asmProgram =
  Translator(List(mainSource, doubleSource)).translate

val asmInstructions = asmProgram.collect:
  case inst: asm.Inst => inst

val symbolTable =  asm.symbolTable(asmProgram)

symbolTable.underlying foreach println

// val endAddress = symbolTable(asm.UserSymbol("END"))

val endAddress = symbolTable(asm.UserSymbol(".END_LOOP"))

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

states.zipWithIndex.take(500).foreach: (state, clock) =>
  val pointers = state.ram.underlying.slice(0, 5)
  val temp = state.ram.underlying.slice(13, 15)
  val stack = state.ram.underlying.slice(256, state.sp.toInt)
  println(s"$clock:${state.pc} ${pointers.mkString(" ")} | ${temp.mkString(" ")} | ${stack.mkString(" ")}")


//
