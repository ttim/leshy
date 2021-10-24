package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

object LshImpl {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )

  def createInterpreter(debug: Boolean): Interpreter =
    new Interpreter(FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath)), debug)

  def fib4(interpreter: Interpreter, value: Int): Int =
    interpreter.run("fib4") { stack =>
      stack.append(Bytes.fromInt(value), isConst = false)
    }.asInt.get

  def fib8(interpreter: Interpreter, value: Int): Long =
    interpreter.run("fib8") { stack =>
      stack.append(Bytes.fromInt(value), isConst = false)
    }.asLong.get

  def fibx4(interpreter: Interpreter, value: Int): Int =
    fibx(interpreter, 4, value).asInt.get

  def fibx8(interpreter: Interpreter, value: Int): Long =
    fibx(interpreter, 8, value).asLong.get

  def fibx(interpreter: Interpreter, length: Int, input: Int): Bytes = {
    val output = interpreter.run("fibx") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }
    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial(interpreter: Interpreter, length: Int, input: Int): Bytes = {
    val output = interpreter.run("ffactorial") { stack =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }
    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial4(interpreter: Interpreter, input: Int): Int =
    ffactorial(interpreter, 4, input).asInt.get

  def ffactorial8(interpreter: Interpreter, input: Int): Long =
    ffactorial(interpreter, 8, input).asLong.get

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(fib4(createInterpreter(false), 38))
    println(ffactorial8(createInterpreter(true), 17))
    println((System.currentTimeMillis() - start)/1000)
  }
}
