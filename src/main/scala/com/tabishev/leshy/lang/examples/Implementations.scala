package com.tabishev.leshy.lang.examples

import com.tabishev.leshy.lang.common.FnSpec
import com.tabishev.leshy.lang.compiler.Compiler

object Implementations {
  private val interpreter = FnSpecs.createInterpreter(debug = false, checkConsts = true)
  private val interpreterNoCheck = FnSpecs.createInterpreter(debug = false, checkConsts = false)

  val Fib4: Map[String, Int => Int] = Map(
    "specialized/interpreter-check" -> interpreter.run(FnSpecs.Fib4),
    "specialized/interpreter-no-check" -> interpreterNoCheck.run(FnSpecs.Fib4),
    "specialized/compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Fib4, doInlining = false, doBytecodeGeneration = false),
    "specialized/compiler-inline-no-gen" -> FnSpecs.createCompiler(FnSpecs.Fib4, doInlining = true, doBytecodeGeneration = false),
    "specialized/compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Fib4, doInlining = false, doBytecodeGeneration = true),
    "specialized/compiler-inline-gen" -> FnSpecs.createCompiler(FnSpecs.Fib4, doInlining = true, doBytecodeGeneration = true),
    "generic/interpreter-check" -> interpreter.run(FnSpecs.Fibx4),
    "generic/interpreter-no-check" -> interpreterNoCheck.run(FnSpecs.Fibx4),
    "generic/compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Fibx4, doInlining = false, doBytecodeGeneration = false),
    "generic/compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Fibx4, doInlining = false, doBytecodeGeneration = true),
    "native" -> (n => JavaImpl.fib4(n)),
  )

  val Fib8: Map[String, Int => Long] = Map(
    "specialized/interpreter-check" -> interpreter.run(FnSpecs.Fib8),
    "specialized/interpreter-no-check" -> interpreterNoCheck.run(FnSpecs.Fib8),
    "specialized/compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Fib8, doInlining = false, doBytecodeGeneration = false),
    "specialized/compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Fib8, doInlining = false, doBytecodeGeneration = true),
    "generic/interpreter-check" ->interpreter.run(FnSpecs.Fibx8),
    "generic/interpreter-no-check" ->interpreterNoCheck.run(FnSpecs.Fibx8),
    "generic/compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Fibx8, doInlining = false, doBytecodeGeneration = false),
    "generic/compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Fibx8, doInlining = false, doBytecodeGeneration = true),
    "native" -> (n => JavaImpl.fib8(n)),
  )

  val Fact4: Map[String, Int => Int] = Map(
    "interpreter-check" -> interpreter.run(FnSpecs.Ffactorial4),
    "interpreter-no-check" -> interpreterNoCheck.run(FnSpecs.Ffactorial4),
    "compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Ffactorial4, doInlining = false, doBytecodeGeneration = false),
    "compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Ffactorial4, doInlining = false, doBytecodeGeneration = true),
    "native" -> (n => JavaImpl.ffactorial4(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial4(n)),
  )

  val Fact8: Map[String, Int => Long] = Map(
    "interpreter-check" ->interpreter.run(FnSpecs.Ffactorial8),
    "interpreter-no-check" ->interpreterNoCheck.run(FnSpecs.Ffactorial8),
    "compiler-no-gen" -> FnSpecs.createCompiler(FnSpecs.Ffactorial8, doInlining = false, doBytecodeGeneration = false),
    "compiler-gen" -> FnSpecs.createCompiler(FnSpecs.Ffactorial8, doInlining = false, doBytecodeGeneration = true),
    "native" -> (n => JavaImpl.ffactorial8(n)),
    "byte buffer" -> (n => ByteBufferImpl.ffactorial8(n)),
  )
}
