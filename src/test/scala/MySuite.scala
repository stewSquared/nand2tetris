// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  def testAssembly(asmFile: String, hackFile: String): Unit =
    test(s"$asmFile should compile to $hackFile"):
      val rawAsm = io.Source.fromResource(asmFile).getLines().mkString("\n")
      val expectedHack = io.Source.fromResource(hackFile).getLines().toList

      val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
      val mlProgram = nand2tetris.asm.toML(asmProgram)
      val generatedBinary = mlProgram.map(_.toBinary)

      val asmInstOnly = asmProgram.collect:
        case inst: nand2tetris.asm.Inst => inst

      // TODO: eventually, this should just be logged in assembler
      println(s"Assembling $asmFile")
      asmInstOnly.zip(mlProgram).take(100).foreach:
        case (asm, ml) =>
          println:
            s"${ml.toBinary} <- $ml <- $asm"

      expectedHack.zip(asmInstOnly.zip(mlProgram)).zipWithIndex.foreach:
        case ((expected, (asm, ml)), i) =>
          val generated = ml.toBinary
          assertEquals(generated, expected, s"line $i: $generated != $expected <- $ml <- $asm")

  def testDeref(asmFile: String, asmLFile: String): Unit =
    test("Dereferencing $asmFile should produce $asmLFile"):
      val rawAsm = io.Source.fromResource(asmFile).getLines().mkString("\n")
      val rawAsmL = io.Source.fromResource(asmLFile).getLines().mkString("\n")
      val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
      val table = nand2tetris.asm.symbolTable(asmProgram)
      val derefProgram = nand2tetris.asm.deref(asmProgram, table)

      val expectedDerefProgram = nand2tetris.asm.Program.parse(rawAsmL)
        .collect:
          case inst: nand2tetris.asm.Inst => inst
      assert(expectedDerefProgram.forall(_.isInstanceOf[nand2tetris.asm.Inst]), "expected deref program should only contain instructions")

      derefProgram.zip(expectedDerefProgram).zipWithIndex.foreach:
        case ((deref, expected), i) =>
          assertEquals(deref, expected, s"line $i: $deref != $expected")

  testAssembly("Max.asm", "Max.hack")
  testAssembly("Add.asm", "Add.hack")
  testAssembly("Rect.asm", "Rect.hack")
  testAssembly("Pong.asm", "Pong.hack")

  testDeref("Max.asm", "MaxL.asm")
  testDeref("Rect.asm", "RectL.asm")
  testDeref("Pong.asm", "PongL.asm")

  test("Dereferencing should be idempotent"):
    val rawAsm = io.Source.fromResource("Max.asm").getLines().mkString("\n")
    val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
    val table = nand2tetris.asm.symbolTable(asmProgram)
    val derefProgram = nand2tetris.asm.deref(asmProgram, table)
    val derefAgain = nand2tetris.asm.deref(derefProgram, table)

    assertEquals(derefProgram, derefAgain)

}
