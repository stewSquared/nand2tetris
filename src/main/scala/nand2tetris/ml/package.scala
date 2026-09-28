package nand2tetris
package ml

import cpu.{U15, Word}
import nand2tetris.ml.Comp.Add
import nand2tetris.ml.Comp.And

type Program = List[Instruction]

sealed trait Instruction:
  def toWord: Word = this match
    case AInst(n) => n.toWord
    case CInst(comp, dest, jump) =>
      val preBits = 0xE000
      val compBits = comp.bits << 6
      val destBits = List(
        if dest.a then 0b100 else 0,
        if dest.d then 0b010 else 0,
        if dest.m then 0b001 else 0
      ).sum << 3
      val jumpBits = jump.ordinal
      Word(preBits | compBits | destBits | jumpBits)

  def toHex: String = f"${toWord.toInt}%04x"
  def toBinary: String = f"${toWord.toInt.toBinaryString.takeRight(16)}%16s".replace(' ', '0')

// last 15 bits of the instruction, 0x0 to 0x7FFF
case class AInst(n: U15) extends Instruction:
  override def toString = s"@$n"

case class Comp(op: Comp.Op, no: Boolean = false, a: Boolean = false):
  val zx = op.x.zero
  val nx = op.x.negate
  val zy = op.y.zero
  val ny = op.y.negate
  val f = op match
    case _: Comp.Add => true
    case _: Comp.And => false

  def negated = this.copy(no = true)
  def fromMem = this.copy(a = true)

  val bits =
    inline def bit(b: Boolean): Int = if b then 1 else 0
    bit(a) << 6
    | bit(zx) << 5
    | bit(nx) << 4
    | bit(zy) << 3
    | bit(ny) << 2
    | bit(f) << 1
    | bit(no) << 0

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

  def calc(xVal: Word, yVal: Word): Word =
    val o = op match
      case Add(x, y) => x.calc(xVal) + y.calc(yVal)
      case And(x, y) => x.calc(xVal) & y.calc(yVal)
    if no then ~o else o

object Comp:
  def fromWord(n: Word): Comp =
    val compBits = (n.toInt >> 6) & 0x7F
    val a = ((compBits >> 6) & 1) == 1
    val zx = ((compBits >> 5) & 1) == 1
    val nx = ((compBits >> 4) & 1) == 1
    val zy = ((compBits >> 3) & 1) == 1
    val ny = ((compBits >> 2) & 1) == 1
    val f = ((compBits >> 1) & 1) == 1
    val no = ((compBits >> 0) & 1) == 1

    val x = Comp.Arg.fromBits(zx, nx)
    val y = Comp.Arg.fromBits(zy, ny)

    Comp(
      op = if f then Add(x, y) else And(x, y),
      no = no,
      a = a
    )

  enum Arg(val zero: Boolean, val negate: Boolean):
    case Id extends Arg(zero = false, negate = false)
    case Zero extends Arg(zero = true, negate = false)
    case Inv extends Arg(zero = false, negate = true)
    case Ones extends Arg(zero = true, negate = true)

    def calc(value: Word): Word = this match
      case Id => value
      case Zero => Word(0)
      case Inv => ~value
      case Ones => Word(-1)

    def `+`(other: Arg): Comp.Op = Comp.Add(this, other)
    def `&`(other: Arg): Comp.Op = Comp.And(this, other)

  object Arg:
    def fromBits(zero: Boolean, negate: Boolean): Arg =
      (zero, negate) match
        case (false, false) => Id
        case (true, false) => Zero
        case (false, true) => Inv
        case (true, true) => Ones

  sealed trait Op:
    def x: Arg
    def y: Arg

  case class Add(x: Arg, y: Arg) extends Op
  case class And(x: Arg, y: Arg) extends Op

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
  def fromWord(n: Word): Dest =
    val destBits = (n.toInt >> 3) & 0b111
    val m = (destBits & 0b001) == 0b001
    val d = (destBits & 0b010) == 0b010
    val a = (destBits & 0b100) == 0b100
    Dest(a=a, d=d, m=m)

enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

object Jump:
  // todo: unify with asm.jump and move to cpu/model package?
  def fromWord(n: Word) = Jump.fromOrdinal(n.toInt & 0b111)

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
  def fromWord(n: Word): CInst = CInst(
    comp = Comp.fromWord(n),
    dest = Dest.fromWord(n),
    jump = Jump.fromWord(n)
  )
