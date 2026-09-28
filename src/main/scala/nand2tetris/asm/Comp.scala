package nand2tetris
package asm

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
      case binOp: BinOp => binOp match
        case Add(Reg.A) => mlc.x.add.y
        case Add(Reg.M) => mlc.x.add.y.fromMem
        case And(Reg.A) => mlc.x.and.y
        case And(Reg.M) => mlc.x.and.y.fromMem
        case Or(Reg.A) => mlc.negX.and.negY.negate
        case Or(Reg.M) => mlc.negX.and.negY.negate.fromMem
        case Sub(Reg.A) => mlc.negX.add.y.negate
        case Sub(Reg.M) => mlc.negX.add.y.negate.fromMem
        case SubFrom(Reg.A) => mlc.negY.add.x.negate
        case SubFrom(Reg.M) => mlc.negY.add.x.negate.fromMem
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
    case s"D+$rhs" => Add(Reg.parseRHS(rhs))
    case s"D-$rhs" => Sub(Reg.parseRHS(rhs))
    case s"$rhs-D" => SubFrom(Reg.parseRHS(rhs))
    case s"D&$rhs" => And(Reg.parseRHS(rhs))
    case s"D|$rhs" => Or(Reg.parseRHS(rhs))
    case reg => Noop(Reg.parse(reg))
