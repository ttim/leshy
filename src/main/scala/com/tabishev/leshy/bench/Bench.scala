package com.tabishev.leshy.bench

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.examples.{ByteBufferImpl, JavaImpl, LshImpl, MemoryAccessImpl}
import com.tabishev.leshy.interpreter.{Interpreter, InterpreterState}
import com.tabishev.leshy.loader.FileLoader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

object Bench {
  @State(Scope.Benchmark)
  class BenchState {
    var interpreter: Interpreter = LshImpl.baseInterpreter(debug = false)
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
    bh.consume(JavaImpl.ffactorial4(10001))

  @Benchmark
  def nativeFactorial8(bh: Blackhole): Unit =
    bh.consume(JavaImpl.ffactorial8(10001))

  @Benchmark
  def interpretFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(LshImpl.ffactorial4(state.interpreter, 10001))

  @Benchmark
  def interpretFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(LshImpl.ffactorial8(state.interpreter, 10001))


  @Benchmark
  def byteBufferFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(ByteBufferImpl.ffactorial4(10001))

  @Benchmark
  def byteBufferFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(ByteBufferImpl.ffactorial8(10001))

  @Benchmark
  def memoryAccessFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(MemoryAccessImpl.ffactorial4(10001))

  @Benchmark
  def memoryAccessFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(MemoryAccessImpl.ffactorial8(10001))
}
