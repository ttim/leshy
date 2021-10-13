package com.tabishev.leshy

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.examples.{JavaImpl, LshImpl}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

class InterpreterSpec extends munit.FunSuite {
  val interpreter = LshImpl.baseInterpreter(false)

  test("fib4 works") {
    testFib(
      n => LshImpl.fib4(interpreter, n),
      n => JavaImpl.fib4(n)
    )
  }

  test("fib8 works") {
    testFib(
      n => LshImpl.fib8(interpreter, n),
      n => JavaImpl.fib8(n)
    )
  }

  test("fibx4 works") {
    testFib(
      n => LshImpl.fibx4(interpreter, n),
      n => JavaImpl.fib4(n)
    )
  }

  test("fibx8 works") {
    testFib(
      n => LshImpl.fibx8(interpreter, n),
      n => JavaImpl.fib8(n)
    )
  }

  test("ffactorial 4 works") {
    testFfactorial(
      n => LshImpl.ffactorial(interpreter, 4, n).asInt.get,
      n => JavaImpl.ffactorial4(n)
    )
  }

  test("ffactorial 8 works") {
    testFfactorial(
      n => LshImpl.ffactorial(interpreter, 8, n).asLong.get,
      n => JavaImpl.ffactorial8(n)
    )
  }

  private def testFib[T](interpret: Int => T, expected: Int => T): Unit =
    (1 to 25).foreach { n =>
      assertEquals(interpret(n), expected(n))
    }

  private def testFfactorial[T](interpret: Int => T, expected: Int => T): Unit =
    (1 to 1000).foreach { n =>
      assertEquals(interpret(n), expected(n))
    }
}
