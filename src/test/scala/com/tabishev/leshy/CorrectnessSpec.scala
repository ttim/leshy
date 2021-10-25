package com.tabishev.leshy

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.examples.{ByteBufferImpl, FnSpecs, JavaImpl, MemoryAccessImpl}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

class CorrectnessSpec extends munit.FunSuite {
  private val interpreter = FnSpecs.createInterpreter(false)
  private val compiler = FnSpecs.createCompiler(false)

  val fib4: Seq[Int => Int] = Seq(
    interpreter.run(FnSpecs.Fib4),
    interpreter.run(FnSpecs.Fibx4),
    compiler.run(FnSpecs.Fib4),
    compiler.run(FnSpecs.Fibx4),
    n => JavaImpl.fib4(n)
  )

  val fib8: Seq[Int => Long] = Seq(
    interpreter.run(FnSpecs.Fib8),
    interpreter.run(FnSpecs.Fibx8),
    compiler.run(FnSpecs.Fib8),
    compiler.run(FnSpecs.Fibx8),
    n => JavaImpl.fib8(n)
  )

  val fact4: Seq[Int => Int] = Seq(
    interpreter.run(FnSpecs.Ffactorial4),
    compiler.run(FnSpecs.Ffactorial4),
    n => JavaImpl.ffactorial4(n),
    n => ByteBufferImpl.ffactorial4(n),
    n => MemoryAccessImpl.ffactorial4(n),
  )

  val fact8: Seq[Int => Long] = Seq(
    interpreter.run(FnSpecs.Ffactorial8),
    compiler.run(FnSpecs.Ffactorial8),
    n => JavaImpl.ffactorial8(n),
    n => ByteBufferImpl.ffactorial8(n),
    n => MemoryAccessImpl.ffactorial8(n)
  )

  test("fib4 works") {
    implementationsAgree(inputs = 1 to 25, impls = fib4)
  }

  test("fib8 works") {
    implementationsAgree(inputs = 1 to 25, impls = fib8)
  }

  test("ffactorial4 works") {
    implementationsAgree(inputs = 1 to 1000, impls = fact4)
  }

  test("ffactorial8 works") {
    implementationsAgree(inputs = 1 to 1000, impls = fact8)
  }

  private def implementationsAgree[A, B](inputs: Seq[A], impls: Seq[A => B]): Unit =
    inputs.foreach { input =>
      val outputs = impls.map { impl => impl(input) }
      assert(outputs.forall(_ == outputs.head), s"different outputs for $input: $outputs")
    }
}
