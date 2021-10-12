package com.tabishev.leshy.bench

import com.tabishev.leshy.interpreter.Interpreter.run
import com.tabishev.leshy.interpreter.{InterpreterSession, InterpreterState}
import com.tabishev.leshy.loader.FileLoader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

object Bench {
  @State(Scope.Benchmark)
  class BenchState {
    var interpreter: InterpreterSession = {
      val loader = FileLoader.fromFile(new File("src/main/lsh/factorial.lsh").toPath)
      new InterpreterSession(loader, debug = false)
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
class Bench {
  @Benchmark
  def nativeFactorial4(bh: Blackhole): Unit =
    bh.consume(Example.fact4(33))

  @Benchmark
  def nativeFactorial8(bh: Blackhole): Unit =
    bh.consume(Example.fact8(33))

  @Benchmark
  def interpretFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.interpreter.run("test_fib_4"))

  @Benchmark
  def interpretFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.interpreter.run("test_fib_8"))
}
