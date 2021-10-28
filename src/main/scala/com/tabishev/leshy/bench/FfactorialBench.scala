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
class FfactorialBench {
  @Param(Array("interpreter/generic", "compiler/generic", "native", "byte buffer", "memory access"))
  var impl: String = _

  @Benchmark
  def factorial4(bh: Blackhole): Unit =
    bh.consume(Implementations.Fact4(impl).apply(10001))

  @Benchmark
  def factorial8(bh: Blackhole): Unit =
    bh.consume(Implementations.Fact8(impl).apply(10001))
}
