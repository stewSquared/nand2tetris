val rawAsm = io.Source.fromResource("Max.asm").getLines().mkString("\n")
val rawHack = io.Source.fromResource("Max.hack").getLines().toList

val program = nand2tetris.asm.Program.parse(rawAsm)

program.foreach(println)

val rawInstructions = program.collect:
  case inst: nand2tetris.asm.Inst => inst

rawInstructions.foreach(println)

val table = nand2tetris.asm.symbolTable(program)

val derefProgram = nand2tetris.asm.deref(program, table)

derefProgram.foreach(println)

val mlProgram = nand2tetris.asm.toML(program)

val binaryLines = mlProgram.map(_.toBinary)


binaryLines.zip(mlProgram).zip(derefProgram).zip(rawInstructions).foreach:
  case ((binary, ml), deref) -> raw =>
    println(s"$binary <- $ml <- $deref <- $raw")


//
