package com.tabishev.leshy.examples

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.{Node, NodeContext, OperationRef, SpecializationContext}
import com.tabishev.leshy.interpreter.Interpreter
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}
import com.tabishev.leshy.runtime.Runtime

import java.io.File

object LshNodeImpl {
  val IncludePaths = Seq(
    "src/main/lsh/fib.lsh",
    "src/main/lsh/factorial.lsh"
  )

  private val nodeContext: NodeContext =
    new NodeContext(FileLoader.fromFiles(IncludePaths.map(p => new File(p).toPath)), new Runtime())

  private var node: Node = null

  def ffactorial(length: Int, input: Int): Bytes = {
    val runtime = nodeContext.runtime
    assert(runtime.stack.frameOffset == 0 && runtime.stack.size == 0)
    runtime.stack.append(Bytes.fromInt(length), isConst = true)
    runtime.stack.append(Bytes.fromInt(input), isConst = false)

    if (node == null)
      node = nodeContext.create(OperationRef("ffactorial", 0))
    node.run()

    assert(runtime.stack.frameOffset == 0)
    val output = Bytes.fromBytes(runtime.stack.getCurrentStackFrame())
    assert(output.get().length == (4 + length))
    assert(output.slice(0, 4).asInt.get == length)
    output.slice(4)
  }

  def ffactorial4(input: Int): Int = ffactorial(4, input).asInt.get

  def ffactorial8(input: Int): Long = ffactorial(8, input).asLong.get

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(ffactorial4(4)) // 8
    println(ffactorial4(5)) // 15
    println((System.currentTimeMillis() - start)/1000)
  }
}
