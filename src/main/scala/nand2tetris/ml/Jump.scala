package nand2tetris.ml

import nand2tetris.cpu.Word

enum Jump:
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP

object Jump:
  // todo: unify with asm.jump and move to cpu/model package?
  def fromWord(n: Word) = Jump.fromOrdinal(n.toInt & 0b111)
