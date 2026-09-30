package nand2tetris
package asm

import nand2tetris.ml.{ Dest, Jump }
import nand2tetris.cpu.U15

sealed trait Inst:
  override def toString: String = this match
    case AInst.Ref(xxx) => s"@$xxx"
    case AInst.Val(xxx) => s"@$xxx"
    case CInst(dest, comp, jump) =>
      val destStr = if dest.isNull then "" else dest.toString + "="
      val jumpStr = if jump == Jump.Null then "" else ";" + jump.toString
      s"$destStr$comp$jumpStr"

  def toML(using table: SymbolTable): ml.Instruction = this match
    case ref: AInst.Ref => table.deref(ref)
    case AInst.Val(c) => ml.AInst(c)
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

sealed trait AInst extends Inst

object AInst:
  case class Ref(sym: Symbol) extends AInst
  case class Val(c: U15) extends AInst

  def parse(raw: String): AInst = raw match
    case s"@$xxx" =>
      // TODO: handle error for bad int
      xxx.toIntOption.map(n => AInst.Val(U15(n)))
        .getOrElse(Ref(Symbol.parse(xxx)))
    case _ => throw new IllegalArgumentException("A-inst must start with @")

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
