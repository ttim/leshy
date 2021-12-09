package com.tabishev.leshy.lang.examples

import com.tabishev.leshy.runtime.{Bytes, StackMemory}
import com.tabishev.leshy.lang.common
import com.tabishev.leshy.lang.common.{FnSpec, Input, Symbols}
import com.tabishev.leshy.lang.compiler.Compiler
import com.tabishev.leshy.lang.interpreter.Interpreter
import com.tabishev.leshy.lang.loader.{FileLoader, FnLoader}

import java.io.File

object FnSpecs {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )
  def loader(): FnLoader = FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath))

  def createInterpreter(debug: Boolean, checkConsts: Boolean): Interpreter =
    new Interpreter(loader(), debug, checkConsts)

  def createCompiler[V](spec: FnSpec[Int, V], doInlining: Boolean, doBytecodeGeneration: Boolean): Int => V =
    Compiler.runner(loader(), new StackMemory(), new Symbols(), debugEnabled = false, doInlining, doBytecodeGeneration, spec, Some(10))

  val Fib4: FnSpec[Int, Int] = common.FnSpec("fib4", { input =>
    Input.add(Bytes.fromInt(input), isConst = false)
  }, _.asInt)

  val Fib8: FnSpec[Int, Long] = common.FnSpec("fib8", { input =>
    Input.add(Bytes.fromInt(input), isConst = false)
  }, _.asLong)

  val Fibx4: FnSpec[Int, Int] = fibx(4).map(_.asInt)

  val Fibx8: FnSpec[Int, Long] = fibx(8).map(_.asLong)

  val Ffactorial4: FnSpec[Int, Int] = ffactorial(4).map(_.asInt)

  val Ffactorial8: FnSpec[Int, Long] = ffactorial(8).map(_.asLong)

  private def fibx(length: Int): FnSpec[Int, Bytes] =
    common.FnSpec("fibx", { input =>
      Input
        .add(Bytes.fromInt(length), isConst = true)
        .add(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  private def ffactorial(length: Int): FnSpec[Int, Bytes] =
    common.FnSpec("ffactorial", { input =>
      Input
        .add(Bytes.fromInt(length), isConst = true)
        .add(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  def main(args: Array[String]): Unit = {
    val fib4 = createCompiler(Fib4, doInlining = true, doBytecodeGeneration = true)
    fib4(39)

    val start = System.currentTimeMillis()
//    println(createInterpreter(false, true).run(Fib4)(30))
//    println(createInterpreter(true, true).run(Ffactorial8)(17))
    assert(fib4(39) == 63245986)
    //    assert(createCompiler(Fibx8, doBytecodeGeneration = true)(39) == 63245986)
    println((System.currentTimeMillis() - start)/1000.0)
  }
}
