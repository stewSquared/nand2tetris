package nand2tetris
package asm

import nand2tetris.ml.{ Dest, Jump }

sealed trait Inst:
  override def toString: String = this match
    case AInst(xxx) => s"@$xxx"
    case CInst(dest, comp, jump) =>
      val destStr = if dest.isNull then "" else dest.toString + "="
      val jumpStr = if jump == Jump.Null then "" else ";" + jump.toString
      s"$destStr$comp$jumpStr"

  def toML: ml.Instruction = this match
    case AInst(sym: Symbol) => throw new Exception("program not dereferenced") // TODO make this not compile
    case AInst(adr: Address) => ml.AInst(adr)
    case CInst(dest, comp, jump) =>
      ml.CInst(
        comp = comp.toML,
        dest = dest,
        jump = jump
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

case class CInst(
  dest: Dest,
  comp: Comp,
  jump: Jump
) extends Inst

object CInst:
  def parse(raw: String): CInst =
    def parseDest(raw: String): (Dest, String) =
      if raw.contains("=") then
        val (dest, rem) = raw.splitAt(raw.indexOf("="))
        Dest.parse(dest) -> rem.drop(1)
      else
        Dest.Null -> raw

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
