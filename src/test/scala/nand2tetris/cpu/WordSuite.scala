package nand2tetris
package cpu
// import for assertThrows

class WordSuite extends munit.FunSuite:
  test("Word should normalize to signed 16 bit"):
    assertEquals(Word(0x8000).toInt, -0x8000)
    assertEquals(Word(0x8000).toInt, 0xFFFF8000)
    assertEquals(Word(-0x8000).toInt, -0x8000)

    assertEquals(Word(0xFFFF).toInt, -1)
    assertEquals(Word(-1).toInt, -1)
    assertEquals(Word(0x7FFF).toInt, 0x7FFF)

    intercept[IllegalArgumentException](Word(0x10000))
    intercept[IllegalArgumentException](Word(-0x8001))

  test("signed 16 bit negatives should work"):
    assertEquals((Word(1).unary_-), Word(-1))
    assertEquals((Word(1).unary_-), Word(0xFFFF))
    assertEquals((Word(1).unary_-).toInt, -1)

    assertEquals((Word(0x7FFF).unary_-), Word(0x8001))
    assertEquals((Word(0x7FFF).unary_-).toInt, -0x7FFF)

    assertEquals((Word(0x8000).unary_-), Word(0x8000)) // Notice!
    assertEquals((Word(0x8000).unary_-).toInt, -0x8000)

    assertEquals((Word(0x0).unary_-), Word(0x0))
    assertEquals((Word(0x0).unary_-).toInt, 0x0)

  test("signed 16 bit ~ should work"):
    assertEquals((Word(0x0001).unary_~), Word(0xFFFE))
    assertEquals((Word(0xFFFE).unary_~), Word(0x0001))

    assertEquals((Word(0x0000).unary_~), Word(0xFFFF))
    assertEquals((Word(0xFFFF).unary_~), Word(0x0000))

    assertEquals((Word(0x7FFF).unary_~), Word(0x8000))
    assertEquals((Word(0x8000).unary_~), Word(0x7FFF))

  test("U15 should normalize to unsigned 15 bit"):
    assertEquals(U15(0x7FFF).toInt, 0x7FFF)
    assertEquals(U15(0x0000).toInt, 0x0000)

    intercept[IllegalArgumentException](U15(0x8000))
    intercept[IllegalArgumentException](U15(-1))

  test("U15 increment overflow should wrap around"):
    assertEquals(U15(0x7FFF).inc.toInt, 0x0000)
    assertEquals(U15(0x0000).inc.toInt, 0x0001)

  test("Word & Word should work"):
    assertEquals(Word(0xFFFF) & Word(0x0000), Word(0x0000))
    assertEquals(Word(0xFFFF) & Word(0xAAAA), Word(0xAAAA))
    assertEquals(Word(0xFFFF) & Word(0xFFFF), Word(0xFFFF))
    assertEquals(Word(0xAAAA) & Word(0x5555), Word(0x0000))
    assertEquals(Word(0xAAAA) & Word(0xAAAA), Word(0xAAAA))

  test("Word | Word should work"):
    assertEquals(Word(0xFFFF) | Word(0x0000), Word(0xFFFF))
    assertEquals(Word(0xAAAA) | Word(0x5555), Word(0xFFFF))
    assertEquals(Word(0xAAAA) | Word(0xAAAA), Word(0xAAAA))
    assertEquals(Word(0xAAAA) | Word(0x0000), Word(0xAAAA))

  test("Word addition should work"):
    assertEquals(Word(0x0001) + Word(0x0001), Word(0x0002))
    assertEquals(Word(0x7FFF) + Word(0x0001), Word(0x8000))
    assertEquals(Word(0xFFFF) + Word(0x0001), Word(0x0000))
    assertEquals(Word(0xFFFF) + Word(0xFFFF), Word(0xFFFE))
