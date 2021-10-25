package com.tabishev.leshy.examples

object Implementations {
  private val interpreter = FnSpecs.createInterpreter(false)
  private val compiler = FnSpecs.createCompiler(false)

  val Fib4: Seq[(String, Int => Int)] = Seq(
    "interpreter" -> interpreter.run(FnSpecs.Fib4),
    "interpreter/generic" -> interpreter.run(FnSpecs.Fibx4),
    "compiler" -> compiler.run(FnSpecs.Fib4),
    "compiler/generic" -> compiler.run(FnSpecs.Fibx4),
    "native" -> (n => JavaImpl.fib4(n)),
  )

  val Fib8: Seq[(String, Int => Long)] = Seq(
    "interpreter" -> interpreter.run(FnSpecs.Fib8),
    "interpreter/generic" ->interpreter.run(FnSpecs.Fibx8),
    "compiler" -> compiler.run(FnSpecs.Fib8),
    "compiler/generic" -> compiler.run(FnSpecs.Fibx8),
    "native" -> (n => JavaImpl.fib8(n)),
  )

  val Fact4: Seq[(String, Int => Int)] = Seq(
    "interpreter/generic" ->interpreter.run(FnSpecs.Ffactorial4),
    "compiler/generic" -> compiler.run(FnSpecs.Ffactorial4),
    "native" -> (n => JavaImpl.ffactorial4(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial4(n)),
    "memory access" -> (n => MemoryAccessImpl.ffactorial4(n)),
  )

  val Fact8: Seq[(String, Int => Long)] = Seq(
    "interpreter/generic" ->interpreter.run(FnSpecs.Ffactorial8),
    "compiler/generic" -> compiler.run(FnSpecs.Ffactorial8),
    "native" -> (n => JavaImpl.ffactorial8(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial8(n)),
    "memory access" -> (n => MemoryAccessImpl.ffactorial8(n)),
  )
}
