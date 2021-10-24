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

  private val compiler: Compiler =
    new Compiler(FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath)), new Runtime(), debugEnabled = false)

  def ffactorial(length: Int, input: Int): Bytes = {
    val output = compiler.run("ffactorial") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }

    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial4(input: Int): Int = ffactorial(4, input).asInt.get

  def ffactorial8(input: Int): Long = ffactorial(8, input).asLong.get

  def fib4(input: Int): Int = compiler.run("fib4") { stack =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }.asInt.get

  def fib8(input: Int): Long = compiler.run("fib8") { stack =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }.asLong.get

  def fibx(length: Int, input: Int): Bytes = {
    val output = compiler.run("fibx") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }

    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def fibx4(input: Int): Int = fibx(4, input).asInt.get
  def fibx8(input: Int): Long = fibx(8, input).asLong.get

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()

    assert(fib4(10) == 89)
    assert(fib8(10) == 89)
    assert(fibx4(10) == 89)
    assert(fibx8(10) == 89)

    assert(ffactorial4(4) == 8)
    assert(ffactorial8(4) == 8)

    // todo: move to tests
    compiler.freeze()

    assert(ffactorial8(17) == 34459425)
    assert(ffactorial8(10001) == 7031418319358416161L)
    assert(fibx4(11) == 144)
    assert(fibx8(11) == 144)
//    assert(fibx8(38) == 63245986)
    println(fibx8(38))
    println((System.currentTimeMillis() - start)/1000)
  }
}
