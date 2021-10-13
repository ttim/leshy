package com.tabishev.leshy.bench

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.{Interpreter, InterpreterState}
import com.tabishev.leshy.loader.FileLoader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

object Bench {
  @State(Scope.Benchmark)
  class BenchState {
    var interpreter: Interpreter = {
      val loader = FileLoader.fromFile(new File("src/main/lsh/factorial.lsh").toPath)
      new Interpreter(loader, debug = false)
    }
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
    bh.consume(Example.ffact4(10001))

  @Benchmark
  def nativeFactorial8(bh: Blackhole): Unit =
    bh.consume(Example.ffact8(10001))

  @Benchmark
  def interpretFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.interpreter.run("test_ffact_4", Bytes.Empty))

  @Benchmark
  def interpretFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.interpreter.run("test_ffact_8", Bytes.Empty))
}
