package nand2tetris.ml

type Program = List[Instruction]

type Binary = Int

// TODO: use opaque type
// with hex/binary formatting
// and 16bit validation upon creation
// (need 15 bit validation for constants)

sealed trait Instruction:

  def toBits: Int = this match
    case AInst(n) => n
    case CInst(comp, dest, jump) =>
      val preBits = 0xE000
      val compBits = comp.bits << 6
      val destBits = List(
        if dest.a then 0b100 else 0,
        if dest.d then 0b010 else 0,
        if dest.m then 0b001 else 0
      ).sum << 3
      val jumpBits = jump.ordinal
      preBits | compBits | destBits | jumpBits

  def toHex: String = f"${toBits}%04x"
  def toBinary: String = f"${toBits.toBinaryString}%16s".replace(' ', '0')

// last 15 bits of the instruction, 0x0 to 0x7FFF
case class AInst(n: Binary) extends Instruction:
  override def toString = s"@$n"

// type Comp = Int
case class Comp(
  a: Boolean,
  zx: Boolean,
  nx: Boolean,
  zy: Boolean,
  ny: Boolean,
  f: Boolean, // if true + else &
  no: Boolean
):
  override def toString: String =
    val x =
      val neg = if nx then "~" else ""
      neg + (if zx then "0" else "x")
    val deref = if a then "*" else ""
    val y =
      val neg = if ny then "~" else ""
      neg + (if zy then "0" else deref + "y")

    val binop = if f then s"$x + $y" else s"$x & $y"

    if no then s"~($binop)" else binop


  def bit(b: Boolean): Int = if b then 1 else 0
  def bits: Int = List(
    bit(a) << 6,
    bit(zx) << 5,
    bit(nx) << 4,
    bit(zy) << 3,
    bit(ny) << 2,
    bit(f) << 1,
    bit(no) << 0,
  ).sum

  def fromMem = this.copy(a = true)
  def zeroX = this.copy(zx = true)
  def negX = this.copy(nx = true)
  def zeroY = this.copy(zy = true)
  def negY = this.copy(ny = true)
  def add = this.copy(f = true)
  def and = this.copy(f = false)
  def negate = this.copy(no = true)

  def xNegOne = this.zeroX.negX
  // TODO: synonym? all 1s?
  def yNegOne = this.zeroY.negY
  def x = this.copy(zx = false, nx = false)
  def y = this.copy(zy = false, ny = false)

object Comp:
  def default: Comp = Comp(false, false, false, false, false, false, false)

  def fromBinary(n: Binary): Comp =
    val comp = n >> 6
    Comp(
      a   = ((comp >> 6) & 1) != 0,
      zx     = ((comp >> 5) & 1) != 0,
      nx   = ((comp >> 4) & 1) != 0,
      zy     = ((comp >> 3) & 1) != 0,
      ny   = ((comp >> 2) & 1) != 0,
      f       = ((comp >> 1) & 1) != 0,
      no = ((comp >> 0) & 1) != 0
    )

// maybe I'll simplify this to three possible destinations
// (on null)
// rather than 8
case class Dest(
  a: Boolean,
  d: Boolean,
  m: Boolean// MD=A
):
  override def toString: String =
    List(
      if a then "A" else "",
      if d then "D" else "",
      if m then "M" else ""
    ).mkString("")

  def isNull = !a && !d && !m

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
) extends Instruction:
  override def toString=
    val jumpStr = if jump != Jump.Null then s";${jump}" else ""
    if dest.isNull then s"$comp$jumpStr"
    else s"${dest.toString}=$comp$jumpStr"

object CInst:
  def fromBinary(n: Binary): CInst = CInst(
    comp = Comp.fromBinary(n),
    dest = Dest.fromBinary(n),
    jump = Jump.fromBinary(n)
  )

//
