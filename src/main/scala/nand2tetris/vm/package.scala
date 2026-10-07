package nand2tetris.vm

import nand2tetris.cpu.U15
import nand2tetris.asm
import nand2tetris.ml.Dest

enum Segment:
  case Argument, Local, Static, Constant, This, That, Pointer, Temp

import Segment.*

object Segment:
  val symbolOf = Map(
    Local -> asm.HardcodedSymbol.LCL,
    Argument -> asm.HardcodedSymbol.ARG,
    This -> asm.HardcodedSymbol.THIS,
    That -> asm.HardcodedSymbol.THAT,
  )

sealed trait VMCommand:
  def translate: List[asm.Inst]

case class Push(segment: Segment, index: U15) extends VMCommand:
  override def translate: List[asm.Inst] =
    val lines = segment match
      case Constant =>
        s"""|@$index
            |D=A
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Local|Argument|This|That =>
        s"""|@${Segment.symbolOf(segment)}
            |D=M
            |@$index
            |A=D+A
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Pointer =>
        import asm.HardcodedSymbol.{THIS, THAT}
        require(index.toInt == 0 || index.toInt == 1, s"pointer $index")
        val symbol = if index.toInt == 0 then THIS else THAT
        s"""|@$symbol
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Temp =>
        require((0 to 7).contains(index), s"temp $index")
        s"""|@R${index.toInt+5}
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
      case Static =>
        s"""|@File.$index
            |D=M
            |@SP
            |A=M
            |M=D
            |@SP
            |M=M+1""".stripMargin
    lines.linesIterator.map(asm.Inst.parse).toList

case class Pop(segment: Segment, index: U15) extends VMCommand:
  def translate: List[asm.Inst] =
    val lines = segment match
      case Constant => ???
      case Local|Argument|This|That =>
        s"""|@${Segment.symbolOf(segment)}
            |D=M
            |@$index
            |D=D+A
            |@R13
            |M=D
            |@SP
            |AM=M-1
            |D=M
            |@R13
            |A=M
            |M=D""".stripMargin
      case Pointer =>
        import asm.HardcodedSymbol.{THIS, THAT}
        require(index.toInt == 0 || index.toInt == 1, s"pointer $index")
        val symbol = if index.toInt == 0 then THIS else THAT
        s"""|@SP
            |AM=M-1
            |D=M
            |@$symbol
            |M=D""".stripMargin
      case Temp =>
        require((0 to 7).contains(index.toInt), s"temp $index")
        s"""|@SP
            |AM=M-1
            |D=M
            |@R${index.toInt+5}
            |M=D""".stripMargin
      case Static =>
        s"""|@SP
            |AM=M-1
            |D=M
            |@File.$index
            |M=D""".stripMargin
    lines.linesIterator.map(asm.Inst.parse).toList

enum ArithmeticLogical extends VMCommand:
  case Add, Sub, Neg, Eq, Gt, Lt, And, Or, Not

  override def translate: List[asm.Inst] =
    val lines = this match
      case Add =>
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |M=D+M""".stripMargin
      case Sub =>
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |M=M-D""".stripMargin
      case And =>
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |M=D&M""".stripMargin
      case Or =>
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |M=D|M""".stripMargin
      case Neg =>
        s"""|@SP
            |A=M-1
            |M=-M""".stripMargin
      case Not =>
        s"""|@SP
            |A=M-1
            |M=!M""".stripMargin
      case Eq =>
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |D=M-D
            |@IfEq // Surely this is wrong
            |D;JEQ
            |M=0
            |@END
            |0;JMP
            |IfEq:
            |M=1
            |END:
            |""".stripMargin
      case Gt | Lt => ???
    lines.linesIterator.map(asm.Inst.parse).toList
