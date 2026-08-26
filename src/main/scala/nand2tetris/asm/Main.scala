package nand2tetris
package asm

@main def assemble(asmFile: String): Unit =
  val rawAsm = io.Source.fromFile(asmFile).getLines().mkString("\n")
  val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
  val mlProgram = nand2tetris.asm.toML(asmProgram)
  val generatedBinary = mlProgram.map(_.toBinary)

  // TODO: send this to stdout instead
  val hackFile = asmFile.replaceAll("\\.asm$", ".hack")
  val pw = new java.io.PrintWriter(hackFile)
  generatedBinary.foreach(pw.println)
  pw.close()
