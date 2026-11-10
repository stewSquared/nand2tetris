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
  // def translate(ctx: VMContext): (asm.Program, VMContext)
  def translate: Translation[asm.Program]

case class Push(segment: Segment, index: U15) extends VMCommand:
  override def translate: Translation[asm.Program] = segment match
    case Constant => Translation.emit:
      s"""|@$index
          |D=A
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin
    case Local|Argument|This|That => Translation.emit:
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
    case Pointer => Translation.emit:
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
    case Temp => Translation.emit:
      require((0 to 7).contains(index), s"temp $index")
      s"""|@R${index.toInt+5}
          |D=M
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin
    case Static => Translation.emit:
      s"""|@File.$index
          |D=M
          |@SP
          |A=M
          |M=D
          |@SP
          |M=M+1""".stripMargin

case class Pop(segment: Segment, index: U15) extends VMCommand:
  def translate: Translation[asm.Program] = segment match
    case Constant => ???
    case Local|Argument|This|That => Translation.emit:
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
    case Pointer => Translation.emit:
      import asm.HardcodedSymbol.{THIS, THAT}
      require(index.toInt == 0 || index.toInt == 1, s"pointer $index")
      val symbol = if index.toInt == 0 then THIS else THAT
      s"""|@SP
          |AM=M-1
          |D=M
          |@$symbol
          |M=D""".stripMargin
    case Temp => Translation.emit:
      require((0 to 7).contains(index.toInt), s"temp $index")
      s"""|@SP
          |AM=M-1
          |D=M
          |@R${index.toInt+5}
          |M=D""".stripMargin
    case Static =>
      // for
      //   filename <- Translation.inspect(_.filename)
      // yield asm.Program.parse:
      //   s"""|@SP
      //       |AM=M-1
      //       |D=M
      //       |@${filename}.$index
      //       |M=D""".stripMargin
      Translation.inspect(_.filename).flatMap: filename =>
        Translation.emit:
          s"""|@SP
              |AM=M-1
              |D=M
              |@${filename}.$index
              |M=D""".stripMargin

enum ArithmeticLogical extends VMCommand:
  case Add, Sub, Neg, Eq, Gt, Lt, And, Or, Not

  override def translate: Translation[asm.Program] = this match
    case Add => Translation.emit:
      s"""|@SP
          |AM=M-1
          |D=M
          |A=A-1
          |M=D+M""".stripMargin
    case Sub => Translation.emit:
      s"""|@SP
          |AM=M-1
          |D=M
          |A=A-1
          |M=M-D""".stripMargin
    case And => Translation.emit:
      s"""|@SP
          |AM=M-1
          |D=M
          |A=A-1
          |M=D&M""".stripMargin
    case Or => Translation.emit:
      s"""|@SP
          |AM=M-1
          |D=M
          |A=A-1
          |M=D|M""".stripMargin
    case Neg => Translation.emit:
      s"""|@SP
          |A=M-1
          |M=-M""".stripMargin
    case Not => Translation.emit:
      s"""|@SP
          |A=M-1
          |M=!M""".stripMargin
    case cmp@(Eq|Gt|Lt) =>
      // y is top value, saved to D
      // x is next value, M
      // M-D := x - y
      // so m-d > 0, then x > y
      for
        trueLabel <- Translation.freshLabel(s"$this.TRUE")
        endLabel <- Translation.freshLabel(s"$this.END")
      yield
        val jumpCond = cmp match
          case Eq => "JEQ"
          case Gt => "JGT"
          case Lt => "JLT"
        asm.Program.parse:
          s"""|@SP
              |AM=M-1
              |D=M
              |A=A-1
              |D=M-D
              |@$trueLabel
              |D;$jumpCond
              |@SP
              |A=M-1
              |M=0
              |@$endLabel
              |0;JMP
              |($trueLabel)
              |@SP
              |A=M-1
              |M=-1
              |($endLabel)
              |""".stripMargin

case class Label(label: String) extends VMCommand:
  def translate: Translation[asm.Program] =
    Translation.inspect(_.currentFunction).map: func =>
      asm.Program.parse(s"($func.$label)")

case class GoTo(label: String) extends VMCommand:
  def translate: Translation[asm.Program] =
    Translation.inspect(_.currentFunction).map: func =>
      asm.Program.parse:
        s"""|@$func.$label
            |0;JMP""".stripMargin

case class IfGoTo(label: String) extends VMCommand:
  def translate: Translation[asm.Program] =
    Translation.inspect(_.currentFunction).map: func =>
      asm.Program.parse:
        s"""|@SP
            |AM=M-1
            |D=M
            |@$func.$label
            |D;JGT""".stripMargin
