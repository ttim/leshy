package com.tabishev.leshy

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader

import java.io.File

object Main {
  val fib = FileLoader.fromFile(new File("src/main/lsh/fib.lsh").toPath)
  val factorial = FileLoader.fromFile(new File("src/main/lsh/factorial.lsh").toPath)

  def mainFib(): Unit =
    new Interpreter(fib, debug = false).run("main", Bytes.Empty)

  def mainFactorial(): Unit =
    new Interpreter(factorial, debug = false).run("main", Bytes.Empty)

  def mainFactorialLong(): Unit =
    new Interpreter(factorial, debug = false).run("test_big_factorial", Bytes.Empty)

  def main(args: Array[String]): Unit = mainFactorial()
}
