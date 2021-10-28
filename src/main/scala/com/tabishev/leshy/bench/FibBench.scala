package com.tabishev.leshy.bench

import com.tabishev.leshy.examples.Implementations
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
class FibBench {
  @Param(Array("interpreter", "interpreter/generic", "compiler", "compiler/generic", "native"))
  var impl: String = _

  @Benchmark
  def fib4(bh: Blackhole): Unit =
    bh.consume(Implementations.Fib4(impl).apply(20))

  @Benchmark
  def fib8(bh: Blackhole): Unit =
    bh.consume(Implementations.Fib8(impl).apply(20))
}
