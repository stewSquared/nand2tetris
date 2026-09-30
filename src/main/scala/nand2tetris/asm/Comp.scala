package nand2tetris
package asm

sealed trait Comp:
  import Comp.*

  // TODO: Using it?
  def fromMem: Boolean = this match
    case op: BinOp => op.rhs == Arg.M
    case op: UnOp => op.arg == Arg.M
    case Noop(Arg.M) => true
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
    val mlc = ml.Comp.default
    this match
      case Noop(arg) => arg match
        case Arg.D        => mlc.x.and.yNegOne
        case Arg.A        => mlc.xNegOne.and.y
        case Arg.M        => mlc.xNegOne.and.y.fromMem
      case Const.One    => mlc.xNegOne.add.yNegOne.negate
      case Const.Zero   => mlc.zeroX.add.zeroY
      case Const.NegOne => mlc.xNegOne.add.zeroY
      case Not(Arg.D) => mlc.x.and.yNegOne.negate
      case Not(Arg.A) => mlc.xNegOne.and.y.negate
      case Not(Arg.M) => mlc.xNegOne.and.y.negate.fromMem
      case Neg(Arg.D) => mlc.x.add.yNegOne.negate
      case Neg(Arg.A) => mlc.y.add.xNegOne.negate
      case Neg(Arg.M) => mlc.y.add.xNegOne.negate.fromMem
      case Inc(Arg.D) => mlc.negX.add.yNegOne.negate
      case Inc(Arg.A) => mlc.negY.add.xNegOne.negate
      case Inc(Arg.M) => mlc.negY.add.xNegOne.negate.fromMem
      case Dec(Arg.D) => mlc.x.add.yNegOne
      case Dec(Arg.A) => mlc.y.add.xNegOne
      case Dec(Arg.M) => mlc.y.add.xNegOne.fromMem
      case Add(Arg.A) => ml.Comp.default.x.add.y
      case Add(Arg.M) => ml.Comp.default.x.add.y.fromMem
      case And(Arg.A) => ml.Comp.default.x.and.y
      case And(Arg.M) => ml.Comp.default.x.and.y.fromMem
      case Or(Arg.A) => ml.Comp.default.negX.and.negY.negate
      case Or(Arg.M) => ml.Comp.default.negX.and.negY.negate.fromMem
      case Sub(Arg.A) => ml.Comp.default.negX.add.y.negate
      case SubFrom(Arg.A) => ml.Comp.default.negY.add.x.negate
      case Sub(Arg.M) => ml.Comp.default.negX.add.y.negate.fromMem
      case SubFrom(Arg.M) => ml.Comp.default.negY.add.x.negate.fromMem

object Comp:

  sealed trait Arg

  object Arg:
    case object D extends Arg
    type D = D.type

    enum RHS extends Arg:
      case A, M

    export Arg.RHS.{A, M}

    def parseRHS(raw: String): RHS = Arg.RHS.valueOf(raw)

    def parse(raw: String): Arg = raw match
      case "D" => Arg.D
      case r => parseRHS(r)

  sealed trait UnOp extends Comp:
    def arg: Arg

  sealed trait BinOp extends Comp:
    def rhs: Arg.RHS

  case class Add(rhs: Arg.RHS) extends BinOp
  case class And(rhs: Arg.RHS) extends BinOp
  case class Sub(rhs: Arg.RHS) extends BinOp
  case class SubFrom(rhs: Arg.RHS) extends BinOp
  case class Or(rhs: Arg.RHS) extends BinOp

  case class Inc(arg: Arg) extends UnOp
  case class Dec(arg: Arg) extends UnOp
  case class Neg(arg: Arg) extends UnOp
  case class Not(arg: Arg) extends UnOp

  case class Noop(arg: Arg) extends Comp

  sealed trait Const extends Comp
  object Const:
    case object Zero extends Const
    case object One extends Const
    case object NegOne extends Const

  def parse(raw: String): Comp = raw match
    case "0" => Const.Zero
    case "1" => Const.One
    case "-1" => Const.NegOne
    case s"-$arg" => Neg(Arg.parse(arg))
    case s"!$arg" => Not(Arg.parse(arg))
    case s"$arg+1" => Inc(Arg.parse(arg))
    case s"$arg-1" => Dec(Arg.parse(arg))
    case s"D+$rhs" => Add(Arg.parseRHS(rhs))
    case s"D-$rhs" => Sub(Arg.parseRHS(rhs))
    case s"$rhs-D" => SubFrom(Arg.parseRHS(rhs))
    case s"D&$rhs" => And(Arg.parseRHS(rhs))
    case s"D|$rhs" => Or(Arg.parseRHS(rhs))
    case reg => Noop(Arg.parse(reg))
