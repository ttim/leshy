package com.tabishev.leshy.lang.bench

import com.tabishev.leshy.lang.examples.Implementations
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
class FibBench {
  @Param(Array(
    "specialized/interpreter-check", "specialized/interpreter-no-check", "specialized/compiler-no-gen", "specialized/compiler-gen",
    "generic/interpreter-check", "generic/interpreter-no-check", "generic/compiler-no-gen", "generic/compiler-gen",
    "native"))
  var impl: String = _

  @Benchmark
  def fib4(bh: Blackhole): Unit =
    bh.consume(Implementations.Fib4(impl).apply(20))

  @Benchmark
  def fib8(bh: Blackhole): Unit =
    bh.consume(Implementations.Fib8(impl).apply(20))
}
