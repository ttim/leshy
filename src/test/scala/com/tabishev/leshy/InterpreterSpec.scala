package com.tabishev.leshy

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.bench.Example
import com.tabishev.leshy.examples.Wrapper
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

class InterpreterSpec extends munit.FunSuite {
  val interpreter = Wrapper.baseInterpreter(false)

  test("fib4 works") {
    testFib(
      n => Wrapper.fib4(interpreter, n),
      n => Example.fib4(n)
    )
  }

  test("fib8 works") {
    testFib(
      n => Wrapper.fib8(interpreter, n),
      n => Example.fib8(n)
    )
  }

  test("fibx4 works") {
    testFib(
      n => Wrapper.fibx4(interpreter, n),
      n => Example.fib4(n)
    )
  }

  test("fibx8 works") {
    testFib(
      n => Wrapper.fibx8(interpreter, n),
      n => Example.fib8(n)
    )
  }

  test("ffactorial 4 works") {
    testFfactorial(
      n => Wrapper.ffactorial(interpreter, 4, n).asInt.get,
      n => Example.ffactorial4(n)
    )
  }

  test("ffactorial 8 works") {
    testFfactorial(
      n => Wrapper.ffactorial(interpreter, 8, n).asLong.get,
      n => Example.ffactorial8(n)
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
