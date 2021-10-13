package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

object Wrapper {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )

  def baseInterpreter(debug: Boolean): Interpreter =
    new Interpreter(FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath)), debug)

  def fib4(interpreter: Interpreter, value: Int): Int =
    interpreter.run("fib4", Bytes.fromInt(value)).asInt.get

  def fib8(interpreter: Interpreter, value: Int): Long =
    interpreter.run("fib8", Bytes.fromInt(value)).asLong.get

  def fibx4(interpreter: Interpreter, value: Int): Int =
    fibx(interpreter, 4, value).asInt.get

  def fibx8(interpreter: Interpreter, value: Int): Long =
    fibx(interpreter, 8, value).asLong.get

  def fibx(interpreter: Interpreter, length: Int, input: Int): Bytes = {
    val output = interpreter.run("fibx", Bytes.seq(Bytes.fromInt(length), Bytes.fromInt(input)))
    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial(interpreter: Interpreter, length: Int, input: Int): Bytes = {
    val output = interpreter.run("ffactorial", Bytes.seq(Bytes.fromInt(length), Bytes.fromInt(input)))
    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }
}
