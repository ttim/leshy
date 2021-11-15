package com.tabishev.leshy.examples

object Implementations {
  private val interpreter = FnSpecs.createInterpreter(debug = false, checkConsts = true)
  private val interpreterNoCheck = FnSpecs.createInterpreter(debug = false, checkConsts = false)
  private val compiler = FnSpecs.createCompiler(false, doBytecodeGeneration = false)

  val Fib4: Map[String, Int => Int] = Map(
    "interpreter" -> interpreter.run(FnSpecs.Fib4),
    "interpreter/no-check" -> interpreterNoCheck.run(FnSpecs.Fib4),
    "interpreter/generic" -> interpreter.run(FnSpecs.Fibx4),
    "interpreter/generic/no-check" -> interpreterNoCheck.run(FnSpecs.Fibx4),
    "compiler" -> compiler.run(FnSpecs.Fib4),
    "compiler/generic" -> compiler.run(FnSpecs.Fibx4),
    "native" -> (n => JavaImpl.fib4(n)),
  )

  val Fib8: Map[String, Int => Long] = Map(
    "interpreter" -> interpreter.run(FnSpecs.Fib8),
    "interpreter/no-check" -> interpreterNoCheck.run(FnSpecs.Fib8),
    "interpreter/generic" ->interpreter.run(FnSpecs.Fibx8),
    "interpreter/generic/no-check" ->interpreterNoCheck.run(FnSpecs.Fibx8),
    "compiler" -> compiler.run(FnSpecs.Fib8),
    "compiler/generic" -> compiler.run(FnSpecs.Fibx8),
    "native" -> (n => JavaImpl.fib8(n)),
  )

  val Fact4: Map[String, Int => Int] = Map(
    "interpreter/generic" -> interpreter.run(FnSpecs.Ffactorial4),
    "interpreter/generic/no-check" -> interpreterNoCheck.run(FnSpecs.Ffactorial4),
    "compiler/generic" -> compiler.run(FnSpecs.Ffactorial4),
    "native" -> (n => JavaImpl.ffactorial4(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial4(n)),
    "memory access" -> (n => MemoryAccessImpl.ffactorial4(n)),
  )

  val Fact8: Map[String, Int => Long] = Map(
    "interpreter/generic" ->interpreter.run(FnSpecs.Ffactorial8),
    "interpreter/generic/no-check" ->interpreterNoCheck.run(FnSpecs.Ffactorial8),
    "compiler/generic" -> compiler.run(FnSpecs.Ffactorial8),
    "native" -> (n => JavaImpl.ffactorial8(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial8(n)),
    "memory access" -> (n => MemoryAccessImpl.ffactorial8(n)),
  )
}
