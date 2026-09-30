package nand2tetris.ml

import nand2tetris.cpu.Word

enum Dest:
  case Null, M, D, DM, A, AM, AD, ADM
  def isNull: Boolean = this == Null
  inline def toBits: Int = this.ordinal

  inline def a: Boolean = (toBits & 0b100) == 0b100
  inline def d: Boolean = (toBits & 0b010) == 0b010
  inline def m: Boolean = (toBits & 0b001) == 0b001

object Dest:
  def parse(raw: String): Dest = Dest.valueOf(raw)
  def fromWord(n: Word): Dest = Dest.fromOrdinal(n.toInt)
