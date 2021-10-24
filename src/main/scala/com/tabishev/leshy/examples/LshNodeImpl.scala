package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.{Node, Compiler, OperationRef, SpecializationContext}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}
import com.tabishev.leshy.runtime.{Runtime, StackMemory}

import java.io.File

object LshNodeImpl {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )

  def createCompiler(debug: Boolean): Compiler = {
    val loader = FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath))
    val instance = new Compiler(loader, new Runtime(), debug)

    // warmup to get all nodes ready before freeze
    ffactorial4(instance, 10)
    ffactorial8(instance, 10)
    fib4(instance, 10)
    fib8(instance, 10)
    fibx4(instance, 10)
    fibx8(instance, 10)

    instance.freeze()
    instance
  }

  def ffactorial(compiler: Compiler, length: Int, input: Int): Bytes = {
    val output = compiler.run("ffactorial") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }

    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial4(compiler: Compiler, input: Int): Int = ffactorial(compiler, 4, input).asInt.get

  def ffactorial8(compiler: Compiler, input: Int): Long = ffactorial(compiler, 8, input).asLong.get

  def fib4(compiler: Compiler, input: Int): Int = compiler.run("fib4") { stack =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }.asInt.get

  def fib8(compiler: Compiler, input: Int): Long = compiler.run("fib8") { stack =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }.asLong.get

  def fibx(compiler: Compiler, length: Int, input: Int): Bytes = {
    val output = compiler.run("fibx") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }

    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def fibx4(compiler: Compiler, input: Int): Int = fibx(compiler, 4, input).asInt.get
  def fibx8(compiler: Compiler, input: Int): Long = fibx(compiler, 8, input).asLong.get

  def main(args: Array[String]): Unit = {
    val compiler = createCompiler(false)

    assert(fib4(compiler, 10) == 89)
    assert(fib8(compiler, 10) == 89)
    assert(fibx4(compiler, 10) == 89)
    assert(fibx8(compiler, 10) == 89)

    assert(ffactorial4(compiler, 4) == 8)
    assert(ffactorial8(compiler, 4) == 8)

    assert(ffactorial8(compiler, 17) == 34459425)
    assert(ffactorial8(compiler, 10001) == 7031418319358416161L)
    assert(fibx4(compiler, 11) == 144)
    assert(fibx8(compiler, 11) == 144)

    val start = System.currentTimeMillis()
    assert(fibx8(compiler, 38) == 63245986)
    println((System.currentTimeMillis() - start)/1000)
  }
}
