package com.tabishev.leshy.bench

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.examples.Wrapper
import com.tabishev.leshy.interpreter.{Interpreter, InterpreterState}
import com.tabishev.leshy.loader.FileLoader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

object Bench {
  @State(Scope.Benchmark)
  class BenchState {
    var interpreter: Interpreter = Wrapper.baseInterpreter(debug = false)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
class Bench {
  @Benchmark
  def nativeFactorial4(bh: Blackhole): Unit =
    bh.consume(Example.ffactorial4(10001))

  @Benchmark
  def nativeFactorial8(bh: Blackhole): Unit =
    bh.consume(Example.ffactorial8(10001))

  @Benchmark
  def interpretFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(Wrapper.ffactorial(state.interpreter, 4, 10001).asInt.get)

  @Benchmark
  def interpretFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(Wrapper.ffactorial(state.interpreter, 8, 10001).asLong.get)
}
