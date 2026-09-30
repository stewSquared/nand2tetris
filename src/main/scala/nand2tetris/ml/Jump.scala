package nand2tetris.ml

import nand2tetris.cpu.Word

enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

  def toBits: Int = this.ordinal

object Jump:
  def fromWord(n: Word) = Jump.fromOrdinal(n.toInt & 0b111)

  def parse(raw: String): Jump =
    require(raw != "Null")
    Jump.valueOf(raw)
