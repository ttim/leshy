package com.tabishev.leshy.bench

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.Compiler
import com.tabishev.leshy.examples.{ByteBufferImpl, FnSpecs, JavaImpl, MemoryAccessImpl}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.io.File
import java.util.concurrent.TimeUnit

object Bench {
  @State(Scope.Benchmark)
  class BenchState {
    var interpreter: Interpreter = FnSpecs.createInterpreter(debug = false)
    val compiler: Compiler = FnSpecs.createCompiler(debug = false)
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
    bh.consume(state.interpreter.run(FnSpecs.Ffactorial4)(10001))

  @Benchmark
  def interpretFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.interpreter.run(FnSpecs.Ffactorial8)(10001))

  @Benchmark
  def nodeFactorial4(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.compiler.run(FnSpecs.Ffactorial4)(10001))

  @Benchmark
  def nodeFactorial8(bh: Blackhole, state: Bench.BenchState): Unit =
    bh.consume(state.compiler.run(FnSpecs.Ffactorial8)(10001))

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
