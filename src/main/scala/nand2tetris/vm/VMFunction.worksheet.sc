import nand2tetris.*
import nand2tetris.asm.symbolTable

s"""|int double(int x) {
    |  var d = x + x;
    |  return d;
    |}""".stripMargin

val mainSource =
  val rawCode =
    s"""|function Main.main 0
        |  push constant 10
        |  call Fib.main 1
        |  return""".stripMargin
  val commands = rawCode.linesIterator.map(vm.VMCommand.parse).toVector
  vm.Source("Main", commands)

// def fib(n: Int): Int =
//   if n <= 1 then n
//   else fib(n - 1) + fib(n - 2)

def fib(n: Int): Int = fib(0, 1, n)

def fib(a: Int, b: Int, n: Int): Int =
  if (n == 0) then a
  else fib(b, a+b, n-1)

(0 to 10).map(fib) foreach println

val fibSource =
  val rawCode =
    s"""|function Fib.main 0
        |  push constant 0
        |  push constant 1
        |  push argument 0
        |  call Fib.fib 3
        |  return
        |function Fib.fib 0
        |  push argument 2
        |  push constant 0
        |  eq
        |  if-goto BASE
        |  push argument 1
        |  push argument 0
        |  push argument 1
        |  add
        |  push argument 2
        |  push constant 1
        |  sub
        |  call Fib.fib 3
        |  return
        |  label BASE
        |  push argument 0
        |  return""".stripMargin
  val commands = rawCode.linesIterator.map(vm.VMCommand.parse).toVector
  vm.Source("Fib", commands.toVector)

mainSource.code foreach println

val asmProgram =
  Translator(List(mainSource, fibSource)).translate

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

states.zipWithIndex.take(3000).foreach: (state, clock) =>
  val pointers = state.ram.underlying.slice(0, 5)
  val temp = state.ram.underlying.slice(13, 15)
  val stack = state.ram.underlying.slice(256, state.sp.toInt)
  println(s"$clock:${state.pc} ${pointers.mkString(" ")} | ${temp.mkString(" ")} | ${stack.mkString(" ")}")


//
