package nand2tetris
package cpu

opaque type Word = Int

object Word:
  def apply(n: Int): Word =
    // mask to 16 bits to translate to unsigned
    // but allow -0x8000 to 0xFFFF to allow flexibility in testing
    require(n >= -0x8000 && n <= 0xFFFF, s"Word must be in range [-0x8000, 0xFFFF], got $n" )
    n & 0xFFFF

  extension (w: Word)
    def toInt: Int = w
    def asAddr: U15 =
      require(w <= 0x7FFF, s"Word must be in range [0, 0x7FFF] to be used as an address, got $w")
      w
    def `+`(other: Word): Word = (w + other) & 0xFFFF
    def `&`(other: Word): Word = (w & other) & 0xFFFF
    def `|`(other: Word): Word = (w | other) & 0xFFFF

    def unary_~ : Word = (~w) & 0xFFFF
    def unary_- : Word = (-w) & 0xFFFF // TODO: double check

opaque type U15 = Int

object U15:
  def apply(n: Int): U15 =
    require(n >= 0 && n <= 0x7FFF, s"U15 must be in range [0, 0x7FFF], got $n")
    n & 0x7FFF

  extension (u: U15)
    def toInt: Int = u
    def toWord: Word = u
    def inc: U15 = U15(u + 1)



//
