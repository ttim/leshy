package com.tabishev.leshy

import com.tabishev.leshy.lang.examples.Implementations

class CorrectnessSpec extends munit.FunSuite {
  private val fibMax = 15
  private val ffactMax = 500

  test("fib4 works") {
    implementationsAgree(inputs = 1 to fibMax, impls = Implementations.Fib4)
  }

  test("fib8 works") {
    implementationsAgree(inputs = 1 to fibMax, impls = Implementations.Fib8)
  }

  test("ffactorial4 works") {
    implementationsAgree(inputs = 1 to ffactMax, impls = Implementations.Fact4)
  }

  test("ffactorial8 works") {
    implementationsAgree(inputs = 1 to ffactMax, impls = Implementations.Fact8)
  }

  private def implementationsAgree[A, B](inputs: Seq[A], impls: Map[String, A => B]): Unit =
    inputs.foreach { input =>
      val outputs = impls.map { case (name, impl) => (name, impl(input)) }
      val (_, golden) = outputs.head
      assert(outputs.forall { case (_, output) => output == golden }, s"different outputs for $input: $outputs")
    }
}
