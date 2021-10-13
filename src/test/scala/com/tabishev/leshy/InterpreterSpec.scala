package com.tabishev.leshy

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.examples.{JavaImpl, LshImpl}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

class InterpreterSpec extends munit.FunSuite {
  val interpreter = LshImpl.baseInterpreter(false)

  val fib4: Seq[Int => Int] = Seq(
    (n) => LshImpl.fib4(interpreter, n),
    (n) => LshImpl.fibx4(interpreter, n),
    (n) => JavaImpl.fib4(n)
  )

  val fib8: Seq[Int => Long] = Seq(
    (n) => LshImpl.fib8(interpreter, n),
    (n) => LshImpl.fibx8(interpreter, n),
    (n) => JavaImpl.fib8(n)
  )

  val fact4: Seq[Int => Int] = Seq(
    (n) => LshImpl.ffactorial4(interpreter, n),
    (n) => JavaImpl.ffactorial4(n)
  )

  val fact8: Seq[Int => Long] = Seq(
    (n) => LshImpl.ffactorial8(interpreter, n),
    (n) => JavaImpl.ffactorial8(n)
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
      assert(outputs.forall(_ == outputs.head))
    }
}
