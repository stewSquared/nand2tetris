package nand2tetris
package asm

sealed trait Comp:
// comp left side is 0 or D
// comp right side is 0 or A or M
// TODO: A+M, etc is not possible
// either a validation step, or not representable
// D=D+A
// Note can never add a reg to itself
  def fromMem: Boolean = this match
    case op: BinOp => op.right == Reg.M
    case op: UnOp => op.arg == Reg.M
    case Noop(Reg.M) => true
    case _ => false

sealed trait BinOp extends Comp:
  def left: Reg
  def right: Reg

sealed trait UnOp extends Comp:
  def arg: Reg

// TODO: in Add and AND, left is always D (right?)
case class Add(left: Reg, right: Reg) extends BinOp
case class And(left: Reg, right: Reg) extends BinOp
case class Sub(left: Reg, right: Reg) extends BinOp
case class Or(left: Reg, right: Reg) extends BinOp
case class Inc(arg: Reg) extends UnOp
case class Dec(arg: Reg) extends UnOp
case class Neg(arg: Reg) extends UnOp // Reg: A,D,M
case class Not(arg: Reg) extends UnOp // A,D,M
case class Noop(arg: Reg | Const) extends Comp
sealed trait Const
object Const:
  case object Zero extends Const
  case object One extends Const
  case object NegOne extends Const

object Comp:
  def parse(raw: String): Comp = raw match
    case "0" => Noop(Const.Zero)
    case "1" => Noop(Const.One)
    case "-1" => Noop(Const.NegOne)
    case s"-$arg" => Neg(Reg.parse(arg))
    case s"!$arg" => Not(Reg.parse(arg))
    case s"$arg+1" => Inc(Reg.parse(arg))
    case s"$arg-1" => Dec(Reg.parse(arg))
    // TODO: left is always D? Only with Add and And? Maybe normalize operand order wrt D
    // TODO: Always A or M?
    case s"$lhs+$rhs" => Add(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs-$rhs" => Sub(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs&$rhs" => And(Reg.parse(lhs), Reg.parse(rhs))
    case s"$lhs|$rhs" => Or(Reg.parse(lhs), Reg.parse(rhs))
    case reg => Noop(Reg.parse(reg))
