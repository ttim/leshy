package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.Compiler
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.{FileLoader, FnLoader}
import com.tabishev.leshy.runtime.{FnSpec, Input, Runtime}

import java.io.File

object FnSpecs {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )
  def loader(): FnLoader = FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath))

  def createInterpreter(debug: Boolean, checkConsts: Boolean): Interpreter =
    new Interpreter(loader(), debug, checkConsts)

  def createCompiler[V](spec: FnSpec[Int, V], doBytecodeGeneration: Boolean): Int => V =
    Compiler.runner(loader(), new Runtime(), debugEnabled = false, doBytecodeGeneration, spec, Some(10))

  val Fib4: FnSpec[Int, Int] = FnSpec("fib4", { input =>
    Input.add(Bytes.fromInt(input), isConst = false)
  }, _.asInt)

  val Fib8: FnSpec[Int, Long] = FnSpec("fib8", { input =>
    Input.add(Bytes.fromInt(input), isConst = false)
  }, _.asLong)

  val Fibx4: FnSpec[Int, Int] = fibx(4).map(_.asInt)

  val Fibx8: FnSpec[Int, Long] = fibx(8).map(_.asLong)

  val Ffactorial4: FnSpec[Int, Int] = ffactorial(4).map(_.asInt)

  val Ffactorial8: FnSpec[Int, Long] = ffactorial(8).map(_.asLong)

  private def fibx(length: Int): FnSpec[Int, Bytes] =
    FnSpec("fibx", { input =>
      Input
        .add(Bytes.fromInt(length), isConst = true)
        .add(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  private def ffactorial(length: Int): FnSpec[Int, Bytes] =
    FnSpec("ffactorial", { input =>
      Input
        .add(Bytes.fromInt(length), isConst = true)
        .add(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
//    println(createInterpreter(false, true).run(Fib4)(30))
//    println(createInterpreter(true, true).run(Ffactorial8)(17))
    assert(createCompiler(Fib8, doBytecodeGeneration = true)(40) == 165580141)
//    assert(createCompiler(Fibx8, doBytecodeGeneration = true)(38) == 63245986)
    println((System.currentTimeMillis() - start)/1000.0)
  }
}
