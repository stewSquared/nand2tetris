import nand2tetris.asm.*

val testProgramSource =
  """|// Computes R1=1+...+R0
     |  // i = 0
     |  @i
     |  M=1
     |  // sum = 0
     |  @sum
     |  M=0
     |(LOOP)
     |  // if i>R0 goto STOP
     |  @i
     |  D=M
     |  @R0
     |  D=D-M
     |  @STOP
     |  D;JGT
     |  // sum += i
     |  @i
     |  D=M
     |  @sum
     |  M=D+M
     |  // i++
     |  @i
     |  M=M+1
     |  @LOOP
     |  0;JMP
     |(STOP)
     |  @sum
     |  D=M
     |  @R1
     |  M=D
     |""".stripMargin

Symbol.parse("i")
Symbol.parse("LOOP")
Symbol.parse("R3")
Symbol.parse("LCL")

Constant.parse(1234)
AInst.parse("@1234")
AInst.parse("@sum")

Inst.parse("@i")
Inst.parse("M=1")

Line.parse("(LOOP)")

val program = Program.parse(testProgramSource)

val table = symbolTable(program)

val transformed = deref(program, table)

val mlProgram = toML(program)
mlProgram foreach println

mlProgram.map(_.toHex) foreach println

program foreach println

transformed foreach println

def parse(program: String): List[Line] =
  val rawLines = program.linesIterator
  rawLines.map[Line]:
    line =>
      line.strip() match
        case s"//$comment" => comment
        case s"($label)" =>
          assert(label.forall(_.isUpper), "Uppercase labels by convention")
          Label(label)
        case s"@$constant" if constant.toIntOption.isDefined =>
          AInst(Constant.parse(constant.toInt))
        case s"@$symbol" =>
          AInst(Symbol.parse(symbol))
        case raw@s"$dest=$comp;$jump" =>
          CInst.parse(raw)
          // CInst(
          //   Reg.parseDest(dest),
          //   comp = Comp.parse(comp),
          //   jump = Jump.parse(jump)
          // )
        // case s"$comp;$jump" =>

          //

  .toList

//
