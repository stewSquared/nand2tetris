package nand2tetris
package asm

sealed trait Symbol:
  def isPredef: Boolean = this match
    case _: PredefSymbol => true
    case _ => false

sealed trait PredefSymbol extends Symbol:
  def value: RomAddress

enum VirtualRegister extends PredefSymbol:
  case R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15
  override def value = RomAddress(this.ordinal)

// case object SP extends PredefSymbol { def value = 0 }

enum HardcodedSymbol extends PredefSymbol:
  case SP, LCL, ARG, THIS, THAT, SCREEN, KBD
  override def value = RomAddress:
    this match
      case SP => 0
      case LCL => 1
      case ARG => 2
      case THIS => 3
      case THAT => 4
      case SCREEN => 0x4000
      case KBD => 0x6000

// Note: strips context from symbol -- did it come from label? variable?
// all we know is that it's user-defined
case class UserSymbol(name: String) extends Symbol

// can't have this detail without the whole program context,
// so let's revive this idea once we have context metadata:
// sealed trait UserSymbol extends Symbol
// case class Label(name: String) extends UserSymbol
// case class Var(name: String) extends UserSymbol

object Symbol:
  private val regIndices = (0 to 15).map(_.toString)
  def parse(name: String): Symbol = name match
    case r@s"R$index" if regIndices.contains(index) =>
      VirtualRegister.fromOrdinal(index.toInt)
    case "SP" => HardcodedSymbol.SP
    case "LCL" => HardcodedSymbol.LCL
    case "ARG" => HardcodedSymbol.ARG
    case "THIS" => HardcodedSymbol.THIS
    case "THAT" => HardcodedSymbol.THAT
    case "SCREEN" => HardcodedSymbol.SCREEN
    case "KBD" => HardcodedSymbol.KBD
    case name => UserSymbol(name)
