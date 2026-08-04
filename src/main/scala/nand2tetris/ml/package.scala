package nand2tetris.ml

type Binary = Int

sealed trait Instruction

// last 15 bits of the instruction, 0x0 to 0x7FFF
case class AInst(n: Binary) extends Instruction

// type Comp = Int
case class Comp(
  fromMem: Boolean,
  zeroX: Boolean,
  negateX: Boolean,
  zeroY: Boolean,
  negateY: Boolean,
  add: Boolean, // otherwise &
  negateOut: Boolean
)

object Comp:
  def fromBinary(n: Binary): Comp =
    val comp = n >> 6
    Comp(
      fromMem   = ((comp >> 6) & 1) != 0,
      zeroX     = ((comp >> 5) & 1) != 0,
      negateX   = ((comp >> 4) & 1) != 0,
      zeroY     = ((comp >> 3) & 1) != 0,
      negateY   = ((comp >> 2) & 1) != 0,
      add       = ((comp >> 1) & 1) != 0,
      negateOut = ((comp >> 0) & 1) != 0
    )

case class Dest(
  a: Boolean,
  d: Boolean,
  m: Boolean// MD=A
)
// maybe I'll simplify this to three possible destinations
// (on null)
// rather than 8

object Dest:
  def fromBinary(n: Binary): Dest =
    val destBits = (n >> 3) & 0b111
    val m = (destBits & 0b001) == 0b001
    val d = (destBits & 0b010) == 0b010
    val a = (destBits & 0b100) == 0b100
    Dest(a=a, d=d, m=m)


enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

object Jump:
  def fromBinary(n: Binary) = Jump.fromOrdinal(n & 0b111)

case class CInst(
  comp: Comp,
  dest: Dest,
  jump: Jump
) extends Instruction

object CInst:
  def fromBinary(n: Binary): CInst = CInst(
    comp = Comp.fromBinary(n),
    dest = Dest.fromBinary(n),
    jump = Jump.fromBinary(n)
  )

//
