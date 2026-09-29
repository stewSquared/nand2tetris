package nand2tetris.ml

import nand2tetris.cpu.Word

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
