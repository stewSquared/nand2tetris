package nand2tetris.ml

type Binary = Int

sealed trait Instruction

// last 15 bits of the instruction, 0x0 to 0x7FFF
case class AInst(n: Binary) extends Instruction

// type Comp = Int
case class Comp(
  // TODO: put fromMem here?
  zeroX: Boolean,
  negateX: Boolean,
  zeroY: Boolean,
  negateY: Boolean,
  add: Boolean, // otherwise &
  negateOut: Boolean
)

object Comp:
  def fromBinary(n: Binary): Comp =
    inline val opCode = n >> 6
    Comp(
      zeroX     = (opCode >> 5) & 1 == 1,
      negateX   = (opCode >> 4) & 1 == 1,
      zeroY     = (opCode >> 3) & 1 == 1,
      negateY   = (opCode >> 2) & 1 == 1,
      add       = (opCode >> 1) & 1 == 1,
      negateOut = (opCode >> 0) & 1 == 1
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
    val a = destBits & 0b001
    val d = destBits & 0b010
    val m = destBits & 0b100
    Dest(a=a, d=d, m=m)


enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

object Jump:
  def fromBinary(n: Binary) = Jump.fromOrdinal(n & 0b111)

case class CInst(
  fromMem: Boolean,
  comp: Comp,
  dest: Dest,
  jump: Jump
) extends Instruction

object CInst:
  def fromBinary(n: Binary): CInst = (
    fromMem = (n >> (3 + 3 + 6)) & 1 == 1,
    comp = Comp.fromBinary(n),
    dest = Dest.fromBinary(n),
    jump = Jump.fromBinary(n)
  )

//
