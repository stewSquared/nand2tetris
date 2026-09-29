package nand2tetris.ml

import nand2tetris.cpu.Word

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

  private def bit(b: Boolean): Int = if b then 1 else 0
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

  def fromWord(n: Word): Comp =
    val compBits = (n.toInt >> 6) & 0x7F
    Comp(
      a = ((compBits >> 6) & 1) != 0,
      zx = ((compBits >> 5) & 1) != 0,
      nx = ((compBits >> 4) & 1) != 0,
      zy = ((compBits >> 3) & 1) != 0,
      ny = ((compBits >> 2) & 1) != 0,
      f = ((compBits >> 1) & 1) != 0,
      no = ((compBits >> 0) & 1) != 0
    )
