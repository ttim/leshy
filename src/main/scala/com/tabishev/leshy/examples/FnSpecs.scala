package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.Compiler
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.FileLoader
import com.tabishev.leshy.runtime.{FnSpec, Runtime}

import java.io.File

object FnSpecs {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )

  def createInterpreter(debug: Boolean): Interpreter =
    new Interpreter(FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath)), debug)

  def createCompiler(debug: Boolean): Compiler = {
    val loader = FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath))
    val instance = new Compiler(loader, new Runtime(), debug)

    // warmup to get all nodes ready before freeze
    Seq[FnSpec[Int, _]](Fib4, Fib8, Fibx4, Fibx8, Ffactorial4, Ffactorial8).foreach { spec =>
      instance.run(spec)(10)
    }

    instance.freeze()
    instance
  }

  val Fib4: FnSpec[Int, Int] = FnSpec("fib4", { (input, stack) =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }, _.asInt)

  val Fib8: FnSpec[Int, Long] = FnSpec("fib8", { (input, stack) =>
    stack.append(Bytes.fromInt(input), isConst = false)
  }, _.asLong)

  val Fibx4: FnSpec[Int, Int] = fibx(4).map(_.asInt)

  val Fibx8: FnSpec[Int, Long] = fibx(8).map(_.asLong)

  val Ffactorial4: FnSpec[Int, Int] = ffactorial(4).map(_.asInt)

  val Ffactorial8: FnSpec[Int, Long] = ffactorial(8).map(_.asLong)

  private def fibx(length: Int): FnSpec[Int, Bytes] =
    FnSpec("fibx", { (input, stack) =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  private def ffactorial(length: Int): FnSpec[Int, Bytes] =
    FnSpec("ffactorial", { (input, stack) =>
      stack.append(Bytes.fromInt(length), isConst = true)
      stack.append(Bytes.fromInt(input), isConst = false)
    }, { output =>
      assert(output.get().length == (4 + length))
      assert(output.slice(0, 4).asInt == length)
      output.slice(4)
    })

  private def testInterpreter(): Unit = {
    val start = System.currentTimeMillis()
    println(createInterpreter(false).run(Fib4)(30))
    println(createInterpreter(true).run(Ffactorial8)(17))
    println((System.currentTimeMillis() - start)/1000.0)
  }

  private def testCompiler(): Unit = {
    val compiler = FnSpecs.createCompiler(false)

    assert(compiler.run(Fib4)(10) == 89)
    assert(compiler.run(Fib8)(10) == 89)
    assert(compiler.run(Fibx4)(10) == 89)
    assert(compiler.run(Fibx8)(10) == 89)

    assert(compiler.run(Ffactorial4)(4) == 8)
    assert(compiler.run(Ffactorial8)(4) == 8)

    assert(compiler.run(Ffactorial8)(17) == 34459425)
    assert(compiler.run(Ffactorial8)(10001) == 7031418319358416161L)
    assert(compiler.run(Fibx4)(11) == 144)
    assert(compiler.run(Fibx8)(11) == 144)

    val start = System.currentTimeMillis()
//    assert(compiler.run(Fib8)(40) == 165580141)
    assert(compiler.run(Fibx8)(38) == 63245986)
    println((System.currentTimeMillis() - start)/1000.0)
  }

  def main(args: Array[String]): Unit = {
//    testInterpreter()
    testCompiler()
  }
}
