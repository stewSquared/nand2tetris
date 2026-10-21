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

case class VMContext(
  filename: String,
  compareCount: Int
)

sealed trait VMCommand:
  def translate(ctx: VMContext): (asm.Program, VMContext)

case class Push(segment: Segment, index: U15) extends VMCommand:
  override def translate(ctx: VMContext): (asm.Program, VMContext) =
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
    lines.linesIterator.map(asm.Inst.parse).toList -> ctx

case class Pop(segment: Segment, index: U15) extends VMCommand:
  def translate(ctx: VMContext): (List[asm.Inst], VMContext) =
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
    lines.linesIterator.map(asm.Inst.parse).toList -> ctx

enum ArithmeticLogical extends VMCommand:
  case Add, Sub, Neg, Eq, Gt, Lt, And, Or, Not

  override def translate(ctx: VMContext): (asm.Program, VMContext) =
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
      case cmp@(Eq|Gt|Lt) =>
        // y is top value, saved to D
        // x is next value, M
        // M-D := x - y
        // so m-d > 0, then x > y
        // TODO: eventually, use monadic state-ish syntax
        // for
        //   count <- ctx.freshCount
        // yield
        //   assemblyUsingCount(count)
        val trueLabel = s"Eq.TRUE$$${ctx.compareCount}"
        val endLabel = s"Eq.END$$${ctx.compareCount}"
        val jumpCond = cmp match
          case Eq => "JEQ"
          case Gt => "JGT"
          case Lt => "JLT"
        s"""|@SP
            |AM=M-1
            |D=M
            |A=A-1
            |D=M-D
            |@$trueLabel
            |D;$jumpCond
            |@SP
            |A=A-1
            |M=0
            |@$endLabel
            |0;JMP
            |($trueLabel)
            |A=A-1
            |M=-1
            |($endLabel)
            |""".stripMargin

    val nextCtx = this match
      case Eq|Gt|Lt => ctx.copy(compareCount = ctx.compareCount + 1)
      case _ => ctx

    lines.linesIterator.map(asm.Inst.parse).toList -> nextCtx
