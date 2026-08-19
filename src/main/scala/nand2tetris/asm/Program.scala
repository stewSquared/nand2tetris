package nand2tetris
package asm

type Comment = String
object Comment:
  def apply(s: String): Comment = s

case class Label(sym: UserSymbol)

type Line = Label | Inst | Comment

type Program = List[Line]
// todo DerefProgram should be its own type

object Program:
  def parse(raw: String): Program =
    raw.linesIterator.filter(_.strip.nonEmpty).toList.map(Line.parse)

object Line:
  def parse(raw: String): Line = raw.strip() match
    case s"//$comment" => Comment(comment)
    case s"($name)" => Label(UserSymbol(name))
    case rawInst => Inst.parse(rawInst)
