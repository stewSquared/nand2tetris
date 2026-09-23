package nand2tetris
package asm

class InstSuite extends munit.FunSuite:
  def testComp(asm: String, expected: Int, a: Int = 0, d: Int = 0, m: Int = 0): Unit =
    test(s"$asm should compute D=$expected when a=$a, d=$d, m=$m"):
      val inst = Inst.parse(asm)

      val result = cpu.State.init(
        rom = Vector(inst.toML),
        a = cpu.Word(a),
        d = cpu.Word(d)
      ).setM(cpu.Word(m)).step

      assertEquals(result.d.toInt, expected, s"Failed for $asm with a=$a, d=$d, m=$m")

  testComp("D=D+A", expected = 7, a = 5, d = 2)
  testComp("D=0", expected = 0)
  testComp("D=1", expected = 1)
  testComp("D=-1", expected = -1)
  testComp("D=A", a = 5, expected = 5)
  testComp("D=M", m = 5, expected = 5)
  testComp("D=!D", d = 2, expected = ~2)
  testComp("D=!A", a = 5, expected = ~5)
  testComp("D=!M", m = 5, expected = ~5)
  testComp("D=-D", d = 2, expected = -2)
  testComp("D=-A", a = 5, expected = -5)
  testComp("D=-M", m = 5, expected = -5)
  testComp("D=D+1", d = 2, expected = 3)
  testComp("D=A+1", a = 5, expected = 6)
  testComp("D=M+1", m = 5, expected = 6)
  testComp("D=D-1", d = 2, expected = 1)
  testComp("D=A-1", a = 5, expected = 4)
  testComp("D=M-1", m = 5, expected = 4)
  testComp("D=D+A", d = 2, a = 5, expected = 7)
  testComp("D=D+M", d = 2, m = 5, expected = 7)
  testComp("D=D-A", d = 2, a = 5, expected = -3)
  testComp("D=D-M", d = 2, m = 5, expected = -3)
  testComp("D=A-D", d = 2, a = 5, expected = 3)
  testComp("D=M-D", d = 2, m = 5, expected = 3)
  testComp("D=D&A", d = 2, a = 5, expected = 0)
  testComp("D=D&M", d = 2, m = 5, expected = 0)
  testComp("D=D|A", d = 2, a = 5, expected = 7)
  testComp("D=D|M", d = 2, m = 5, expected = 7)

  test("D=D+A should result in machine code that adds"):
    val inst = Inst.parse("D=D+A")

    val result = cpu.State.init(
      rom = Vector(inst.toML),
      a = cpu.Word(5),
      d = cpu.Word(2)
    ).step

    assertEquals(result.d.toInt, 7)
