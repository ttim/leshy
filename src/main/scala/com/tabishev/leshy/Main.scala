package com.tabishev.leshy

import com.tabishev.leshy.interpreter.Interpreter.run
import com.tabishev.leshy.loader.FileLoader

import java.io.File

object Main {
  def mainFib(): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/fib.lsh").toPath)
    run(loader, "main", debug = false)
  }

  def mainFactorial(): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/factorial.lsh").toPath)
    run(loader, "main", debug = false)
  }

  def mainFactorialLong(): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/factorial.lsh").toPath)
    run(loader, "main_big_factorial", debug = false)
  }

  def main(args: Array[String]): Unit = mainFactorial()
}
