package com.tabishev.leshy.interpreter

import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File

object Interpreter {
  def run(loader: RoutineLoader, name: String): Unit = {
    ???
  }

  def main(args: Array[String]): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/fib.lsh").toPath)
    run(loader, "main")
  }
}
