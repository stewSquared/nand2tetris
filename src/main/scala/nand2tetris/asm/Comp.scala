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

  override def toString = this match
    case Add(left, right) => s"$left+$right"
    case And(left, right) => s"$left&$right"
    case Or(left, right) => s"$left|$right"
    case Sub(left, right) => s"$left-$right"
    case Inc(arg) => s"$arg+1"
    case Dec(arg) => s"$arg-1"
    case Neg(arg) => s"-$arg"
    case Not(arg) => s"!$arg"
    case Noop(arg) => arg.toString

  def toML: ml.Comp =
    val mlc = ml.Comp.default
    this match
      case Noop(arg) => arg match
        case Reg.D        => mlc.x.and.yNegOne
        case Reg.A        => mlc.xNegOne.and.y
        case Reg.M        => mlc.xNegOne.and.y.fromMem
        case Const.One    => mlc.xNegOne.add.yNegOne.negate
        case Const.Zero   => mlc.zeroX.add.zeroY
        case Const.NegOne => mlc.xNegOne.add.zeroY
      case bin: BinOp => bin.toML
      case Not(Reg.D) => mlc.x.and.yNegOne.negate
      case Not(Reg.A) => mlc.xNegOne.and.y.negate
      case Not(Reg.M) => mlc.xNegOne.and.y.negate.fromMem
      case Neg(Reg.D) => mlc.x.add.yNegOne.negate
      case Neg(Reg.A) => mlc.y.add.xNegOne.negate
      case Neg(Reg.M) => mlc.y.add.xNegOne.negate.fromMem
      case Inc(Reg.D) => mlc.negX.add.yNegOne.negate
      case Inc(Reg.A) => mlc.negY.add.xNegOne.negate
      case Inc(Reg.M) => mlc.negY.add.xNegOne.negate.fromMem
      case Dec(Reg.D) => mlc.x.add.yNegOne
      case Dec(Reg.A) => mlc.y.add.xNegOne
      case Dec(Reg.M) => mlc.y.add.xNegOne.fromMem

sealed trait BinOp extends Comp:
  require(left == Reg.D ^ right == Reg.D, s"BinOp must have exactly one D operand, got $this")
  def left: Reg
  def right: Reg

  override def toML: ml.Comp = this match
    case Add(_, Reg.A) => ml.Comp.default.x.add.y
    case Add(_, Reg.M) => ml.Comp.default.x.add.y.fromMem
    case And(_, Reg.A) => ml.Comp.default.x.and.y
    case And(_, Reg.M) => ml.Comp.default.x.and.y.fromMem
    case Or(_, Reg.A) => ml.Comp.default.negX.and.negY.negate
    case Or(_, Reg.M) => ml.Comp.default.negX.and.negY.negate.fromMem
    case Add(reg, Reg.D) => Add(Reg.D, reg).toML
    case Sub(Reg.D, Reg.A) => ml.Comp.default.negX.add.y.negate
    case Sub(Reg.A, Reg.D) => ml.Comp.default.negY.add.x.negate
    case Sub(Reg.D, Reg.M) => ml.Comp.default.negX.add.y.negate.fromMem
    case Sub(Reg.M, Reg.D) => ml.Comp.default.negY.add.x.negate.fromMem
    case _ => throw new Exception(s"Unsupported BinOp: $this") // should be dead code


sealed trait UnOp extends Comp:
  def arg: Reg

// TODO: Scope these inside of Comp
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
