package nand2tetris.vm

case class Config(initSp: Int = 0x100)

case class Source(name: String, code: Vector[VMCommand])
