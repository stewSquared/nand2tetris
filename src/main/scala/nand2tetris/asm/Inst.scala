package nand2tetris
package asm

sealed trait Inst:
  override def toString: String = this match
    case AInst(xxx) => s"@$xxx"
    case CInst(dest, comp, jump) =>
      val destStr = if dest.isEmpty then "" else dest.map(_.toString).mkString + "="
      val jumpStr = if jump == Jump.Null then "" else ";" + jump.toString
      s"$destStr$comp$jumpStr"

  def toML: ml.Instruction = this match
    case AInst(sym: Symbol) => throw new Exception("program not dereferenced") // TODO make this not compile
    case AInst(adr: Address) => ml.AInst(adr)
    case CInst(dest, comp, jump) =>
      ml.CInst(
        comp = comp.toML,
        dest = ml.Dest(
          a = dest.contains(Reg.A),
          d = dest.contains(Reg.D),
          m = dest.contains(Reg.M)
        ),
        jump = jump.toML
      )

object Inst:
  def parse(raw: String): Inst =
    if raw.startsWith("@") then AInst.parse(raw)
    else CInst.parse(raw)

case class AInst(xxx: Constant | (Symbol | Address)) extends Inst

object AInst:
  def parse(raw: String): AInst = raw match
    case s"@$xxx" =>
      AInst:
        xxx.toIntOption.map: n =>
          Constant.parse(n)
        .getOrElse(Symbol.parse(xxx))
    case _ => throw new Exception("A-inst must start with @")

enum Reg:
  case A, D, M

object Reg:
  def parse(raw: String): Reg = Reg.valueOf(raw)

  val noDest: Set[Reg] = Set.empty
  def parseDest(raw: String): Set[Reg] =
    assert(raw.length <= 3)
    assert(raw.nonEmpty)
    val regs = raw.map(c => Reg.parse(c.toString))
    // assert(regs.unique.size == raw.size)
    regs.toSet

enum Jump:
  // TODO: Does it make sense to Unify ml.Jump and asm.Jump?
  case Null, JGT, JEQ, JGE, JLT, JNE, JLE, JMP
  def toML: ml.Jump = ml.Jump.fromOrdinal(this.ordinal)

object Jump:
  def parse(raw: String): Jump =
    require(raw != "Null")
    Jump.valueOf(raw)

case class CInst(
  dest: Set[Reg],
  comp: Comp,
  jump: Jump // if null, don't show in string
) extends Inst

object CInst:
  def parse(raw: String): CInst =
    def parseDest(raw: String): (Set[Reg], String) =
      if raw.contains("=") then
        val (dest, rem) = raw.splitAt(raw.indexOf("="))
        Reg.parseDest(dest) -> rem.drop(1)
      else
        Reg.noDest -> raw

    def parseJump(raw: String): (Jump, String) =
      if raw.contains(";") then
        val (rem, jump) = raw.splitAt(raw.indexOf(";"))
        Jump.parse(jump.drop(1)) -> rem
      else
        Jump.Null -> raw

    import util.chaining.*

    parseDest(raw).pipe: (dest, rem) =>
      parseJump(rem).pipe: (jump, rem) =>
        CInst(
          dest = dest,
          comp = Comp.parse(rem),
          jump = jump
        )
