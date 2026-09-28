package nand2tetris
package asm

import nand2tetris.asm.Reg.D

sealed trait Comp:
  def fromMem: Boolean = this match
    case op: BinOp => op.arg == Reg.M
    case op: UnOp => op.arg == Reg.M
    case Noop(Reg.M) => true
    case _ => false

  override def toString = this match
    case Add(rhs) => s"D+$rhs"
    case And(rhs) => s"D&$rhs"
    case Or(rhs) => s"D|$rhs"
    case Sub(rhs) => s"D-$rhs"
    case SubFrom(rhs) => s"$rhs-D"
    case Inc(arg) => s"$arg+1"
    case Dec(arg) => s"$arg-1"
    case Neg(arg) => s"-$arg"
    case Not(arg) => s"!$arg"
    case Noop(arg) => arg.toString
    case Const.Zero => "0"
    case Const.One => "1"
    case Const.NegOne => "-1"

  def toML: ml.Comp =
    import ml.Comp.Arg.{Id, Zero, Inv, Ones}
    import ml.Comp.{ Add => MLAdd, And => MLAnd }
    this match
      case Noop(arg) => arg match
        case Reg.D        => ml.Comp(Id & Ones)
        case Reg.A        => ml.Comp(Ones & Id)
        case Reg.M        => ml.Comp(Ones & Id).fromMem
      case Const.One    => ml.Comp(Ones + Ones).negated
      case Const.Zero   => ml.Comp(Zero + Zero)
      case Const.NegOne => ml.Comp(Ones + Zero)
      // Binary Ops
      case Add(Reg.A) => ml.Comp(Id + Id)
      case Add(Reg.M) => ml.Comp(Id + Id).fromMem
      case And(Reg.A) => ml.Comp(Id & Id)
      case And(Reg.M) => ml.Comp(Id & Id).fromMem
      case Or(Reg.A) => ml.Comp(Inv & Inv).negated
      case Or(Reg.M) => ml.Comp(Inv & Inv).negated.fromMem
      case Sub(Reg.A) => ml.Comp(Inv + Id).negated
      case Sub(Reg.M) => ml.Comp(Inv + Id).negated.fromMem
      case SubFrom(Reg.A) => ml.Comp(Id + Inv).negated
      case SubFrom(Reg.M) => ml.Comp(Id + Inv).negated.fromMem
      // Unary Ops
      case Not(Reg.D) => ml.Comp(Id & Ones).negated
      case Not(Reg.A) => ml.Comp(Ones & Id).negated
      case Not(Reg.M) => ml.Comp(Ones & Id).negated.fromMem
      case Neg(Reg.D) => ml.Comp(Id + Ones).negated
      case Neg(Reg.A) => ml.Comp(Ones + Id).negated
      case Neg(Reg.M) => ml.Comp(Ones + Id).negated.fromMem
      case Inc(Reg.D) => ml.Comp(Inv + Ones).negated
      case Inc(Reg.A) => ml.Comp(Ones + Inv).negated
      case Inc(Reg.M) => ml.Comp(Ones + Inv).negated.fromMem
      case Dec(Reg.D) => ml.Comp(Id + Ones)
      case Dec(Reg.A) => ml.Comp(Ones + Id)
      case Dec(Reg.M) => ml.Comp(Ones + Id).fromMem

sealed trait BinOp extends Comp:
  val arg: Reg.AM

sealed trait UnOp extends Comp:
  def arg: Reg

// TODO: Scope these inside of Comp

case class Add(arg: Reg.AM) extends BinOp
case class And(arg: Reg.AM) extends BinOp
case class Sub(arg: Reg.AM) extends BinOp
case class SubFrom(arg: Reg.AM) extends BinOp
case class Or(arg: Reg.AM) extends BinOp
case class Inc(arg: Reg) extends UnOp
case class Dec(arg: Reg) extends UnOp
case class Neg(arg: Reg) extends UnOp
case class Not(arg: Reg) extends UnOp
case class Noop(arg: Reg) extends Comp
sealed trait Const extends Comp
object Const:
  case object Zero extends Const
  case object One extends Const
  case object NegOne extends Const

object Comp:
  def parse(raw: String): Comp = raw match
    case "0" => Const.Zero
    case "1" => Const.One
    case "-1" => Const.NegOne
    case s"-$arg" => Neg(Reg.parse(arg))
    case s"!$arg" => Not(Reg.parse(arg))
    case s"$arg+1" => Inc(Reg.parse(arg))
    case s"$arg-1" => Dec(Reg.parse(arg))
    case s"D+$rhs" => Add(Reg.parseRHS(rhs))
    case s"D-$rhs" => Sub(Reg.parseRHS(rhs))
    case s"$rhs-D" => SubFrom(Reg.parseRHS(rhs))
    case s"D&$rhs" => And(Reg.parseRHS(rhs))
    case s"D|$rhs" => Or(Reg.parseRHS(rhs))
    case reg => Noop(Reg.parse(reg))
