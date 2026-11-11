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
  def pushD: asm.Program = asm.Program.parse:
    s"""|// push D to stack
        |@SP
        |AM=M+1 // increments stack, points past stack
        |A=A-1 // point at value stored on stack
        |M=D""".stripMargin

  override def translate: Translation[asm.Program] = segment match
    case Constant => Translation.pure:
      val writeConstantToD = asm.Program.parse:
        s"""|@$index
            |D=A""".stripMargin
      writeConstantToD ++ pushD
    case Local|Argument|This|That => Translation.pure:
      val writeSegmentItoD = asm.Program.parse:
        s"""|// save segment i to D
            |@${Segment.symbolOf(segment)} // A=1/2/3/4
            |D=M // D points to the base address of segment
            |@$index
            |A=D+A // A points to base+index of segment
            |D=M"""
      writeSegmentItoD ++ pushD
    case Pointer => Translation.pure:
      val writePointerToD = asm.Program.parse:
        import asm.HardcodedSymbol.{THIS, THAT}
        require(index.toInt == 0 || index.toInt == 1, s"pointer $index")
        val symbol = if index.toInt == 0 then THIS else THAT
        s"""|@$symbol
            |D=M""".stripMargin
      writePointerToD ++ pushD
    case Temp => Translation.pure:
      require((0 to 7).contains(index), s"temp $index")
      val writeTempToD = asm.Program.parse:
        s"""|@R${index.toInt+5}
            |D=M""".stripMargin
      writeTempToD ++ pushD
    case Static => Translation.pure:
      val writeStaticToD = asm.Program.parse:
        s"""|@File.$index
            |D=M""".stripMargin
      writeStaticToD ++ pushD

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

  private def popYPointX = asm.Program.parse:
    s"""|@SP
        |AM=M-1 // point at y, decrement sp
        |D=M    // D=y
        |A=A-1  // point at x""".stripMargin

  override def translate: Translation[asm.Program] = this match
    case Add => Translation.pure:
      popYPointX :+ asm.Inst.parse("M=D+M")
    case Sub => Translation.pure:
      popYPointX :+ asm.Inst.parse("M=M-D")
    case And => Translation.pure:
      popYPointX :+ asm.Inst.parse("M=D&M")
    case Or => Translation.pure:
      popYPointX :+ asm.Inst.parse("M=D|M")
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
        popYPointX ++ asm.Program.parse:
          s"""|D=M-D
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
