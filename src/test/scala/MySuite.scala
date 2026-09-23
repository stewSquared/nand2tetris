package nand2tetris

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

  test("Add.asm works in the state machine"):
    val rawAsm = io.Source.fromResource("Add.asm").getLines().mkString("\n")
    val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
    val symbolTable = asm.symbolTable(asmProgram)
    val mlProgram = nand2tetris.asm.toML(asmProgram)

    val init = cpu.State.initRom(mlProgram.toVector)
    val states = LazyList.iterate(init)(_.step)
    val end = states(6)

    assertEquals(states(0).a.toInt, 0)
    assertEquals(states(1).a.toInt, 2)
    assertEquals(states(2).d.toInt, 2)
    assertEquals(states(3).a.toInt, 3)
    assertEquals(states(4).d.toInt, 5)
    assertEquals(states(5).a.toInt, 0)
    assertEquals(states(5).m.toInt, 0)
    assertEquals(end.m.toInt, 5)

  test("Max.asm works in the state machine"):
    val rawAsm = io.Source.fromResource("Max.asm").getLines().mkString("\n")
    val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
    val symbolTable = asm.symbolTable(asmProgram)
    val mlProgram = nand2tetris.asm.toML(asmProgram)

    val endAddr = symbolTable(asm.Symbol.parse("END"))

    val romLoaded = cpu.State.initRom(mlProgram.toVector)

    def runMax(r0: Int, r1: Int): cpu.State =
      val state = romLoaded.setR0(cpu.Word(r0)).setR1(cpu.Word(r1))
      Iterator.iterate(state)(_.step)
        .dropWhile(_.pc != endAddr)
        .next()

    // reminder: these are unsigned 15-bit
    assertEquals(runMax(5, 31).r2.toInt, 31)
    assertEquals(runMax(31, 5).r2.toInt, 31)
    // assertEquals(runMax(-0xF, 5).r2, 0x7ff1)
    assertEquals(runMax(0x7FFF, 5).r2.toInt, 0x7FFF)
    // assertThrows[IllegalArgumentException](runMax(0x8000, 5))

  test("Rect.asm works in the state machine"):
    val rawAsm = io.Source.fromResource("Rect.asm").getLines().mkString("\n")
    val asmProgram = nand2tetris.asm.Program.parse(rawAsm)
    val symbolTable = asm.symbolTable(asmProgram)
    val mlProgram = nand2tetris.asm.toML(asmProgram)

    val endAddr = symbolTable(asm.Symbol.parse("END"))

    val init = cpu.State
      .initRom(mlProgram.toVector)
      .setR0(cpu.Word(2))

    val endState = Iterator.iterate(init)(_.step)
      .dropWhile(_.pc != endAddr)
      .next()

    endState.drawSubscreen(0, 0, 17, 4)

    // screen is 512 x 256 pixels, 16 pixels per word, 32 words per row
    assertEquals(endState.ram(cpu.U15(0x4000)).toInt, -1)
    assertEquals(endState.ram(cpu.U15(0x4001)).toInt, 0)
    assertEquals(endState.ram(cpu.U15(0x4000 + 32)).toInt, -1)
    assertEquals(endState.ram(cpu.U15(0x4000 + 33)).toInt, 0)
    assertEquals(endState.ram(cpu.U15(0x4000 + 64)).toInt, 0)
}
