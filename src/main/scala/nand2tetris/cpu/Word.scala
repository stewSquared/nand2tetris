package nand2tetris
package cpu

opaque type Word = Int

object Word:
  def apply(n: Int): Word =
    // mask to 16 bits to translate to unsigned
    // but allow -0x8000 to 0xFFFF to allow flexibility in testing
    require(n >= -0x8000 && n <= 0xFFFF, s"Word must be in range [-0x8000, 0xFFFF], got $n" )
    n << 16 >> 16

  extension (w: Word)
    def toInt: Int = w
    def toU15: Option[U15] =
      Option.when(w >= 0 && w <= 0x7FFF)(U15(w))

    def asAddr: U15 =
      require(w <= 0x7FFF, s"Word must be in range [0, 0x7FFF] to be used as an address, got $w")
      w
    def `+`(other: Word): Word = (w + other) << 16 >> 16
    def `&`(other: Word): Word = (w & other)
    def `|`(other: Word): Word = (w | other)

    def unary_~ : Word = ~w
    def unary_- : Word = Word(-w)

opaque type U15 = Int

object U15:
  def apply(n: Int): U15 =
    require(n >= 0 && n <= 0x7FFF, s"U15 must be in range [0, 0x7FFF], got $n")
    n

  extension (u: U15)
    def toInt: Int = u
    def toWord: Word = u
    def inc: U15 = (u + 1) & 0x7FFF

//
