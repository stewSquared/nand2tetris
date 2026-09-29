package nand2tetris.ml

import nand2tetris.cpu.{U15, Word}

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

case class AInst(n: U15) extends Instruction:
  override def toString = s"@$n"

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
