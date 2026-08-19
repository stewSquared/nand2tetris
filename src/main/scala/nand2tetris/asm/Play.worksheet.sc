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

table foreach println

val transformed = deref(program, table)

val mlProgram = toML(program)
mlProgram foreach println

mlProgram.map(_.toHex) foreach println
mlProgram.map(_.toBinary) foreach println

mlProgram.map: line =>
  line.toBinary -> line
.foreach:
  println

transformed.zip(mlProgram).foreach:
  case (asm, ml) => println:
    s"${ml.toBinary} <- $ml <- $asm"

program foreach println

transformed foreach println

// Label.parse("ITSR0")

Symbol.parse("ITSR0")

val maxAsm = """|  // D = R0 - R1
            |  @R0
            |  D=M
            |  @R1
            |  D=D-M
            |  // If (D > 0) goto ITSR0
            |  @ITSR0
            |  D;JGT
            |  // Its R1
            |  @R1
            |  D=M
            |  @OUTPUT_D
            |  0;JMP
            |(ITSR0)
            |  @R0
            |  D=M
            |(OUTPUT_D)
            |  @R2
            |  M=D
            |(END)
            |  @END
            |  0;JMP""".stripMargin

val maxProgram = Program.parse(maxAsm)
val maxProgramTable = symbolTable(maxProgram)
val maxProgramDeref = deref(maxProgram, symbolTable(maxProgram))
val maxMLProgram = toML(maxProgram)

maxProgramTable foreach println

maxProgramDeref.zip(maxMLProgram).foreach:
  case (asm, ml) =>
    println:
      s"${ml.toBinary} <- $ml <- $asm"

val generatedMaxBinary =
  toML(Program.parse(maxAsm)).map(_.toBinary).mkString("\n")

val maxBinary = """|0000000000000000
                   |1111110000010000
                   |0000000000000001
                   |1111010011010000
                   |0000000000001010
                   |1110001100000001
                   |0000000000000001
                   |1111110000010000
                   |0000000000001100
                   |1110101010000111
                   |0000000000000000
                   |1111110000010000
                   |0000000000000010
                   |1110001100001000
                   |0000000000001110
                   |1110101010000111""".stripMargin

generatedMaxBinary.lines.toList.size
maxBinary.lines.toList.size

maxBinary == generatedMaxBinary

CInst.parse("D=M+1")
CInst.parse("D=M-D")

CInst.parse("D=M-D").comp
toML(List(
  CInst.parse("D=M-D")
))



//
