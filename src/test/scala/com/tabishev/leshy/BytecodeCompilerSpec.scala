package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{Const, Execution, MemoryOperand, Nodes}
import com.tabishev.leshy.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Node}
import com.tabishev.leshy.runtime.{FrameOffset, Runtime}

import java.io.File
import java.util

class BytecodeCompilerSpec extends munit.FunSuite {
  private def genOrigin(): Nodes.Origin = Nodes.Origin(null, null, null)

  private def finalNode(): Node = Nodes.Final(genOrigin())
  private def executeNode(ex: Execution, next: Node): Node = Nodes.Execute(genOrigin(), next, ex)

  test("write4") {
    check(
      executeNode(
        Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero)),
        finalNode()))
  }

  private def check(node: Node): Unit = {
    val expectedRuntime = new Runtime()
    val expectedFinal = node.run(expectedRuntime)
    val expected = expectedRuntime.stack.currentStackFrame()

    val actualRuntime = new Runtime()
    val actualFinal = BytecodeCompiler.compile(node).run(actualRuntime)
    val actual = actualRuntime.stack.currentStackFrame()

    assert(expectedFinal == actualFinal)
    assert(util.Arrays.equals(expected, actual))
  }
}
