package nand2tetris.vm

import nand2tetris.cpu.U15
import nand2tetris.asm
import nand2tetris.ml.Dest

case class VMContext(
  filename: String,
  compareCount: Int,
  currentFunction: String
):
  def incCompareCount = this.copy(compareCount = compareCount + 1)

case class Translation[A](run: VMContext => (A, VMContext)):
  def map[B](f: A => B): Translation[B] = Translation[B]: ctx =>
    val (a, nextCtx) = run(ctx)
    f(a) -> nextCtx

  def flatMap[B](f: A => Translation[B]) = Translation[B]: ctx =>
    val (a, nextCtx) = run(ctx)
    f(a).run(nextCtx)

case object Translation:
  def pure[A](a: A): Translation[A] = Translation(ctx => (a -> ctx))
  def emit(assemblyLines: String): Translation[asm.Program] =
    Translation.pure(asm.Program.parse(assemblyLines))

  def inspect[A](f: VMContext => A): Translation[A] = Translation[A]: ctx =>
    f(ctx) -> ctx

  def freshLabel(description: String): Translation[String] = Translation: ctx =>
    s"$description$$${ctx.compareCount}" -> ctx.incCompareCount
