package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{BranchExecution, Const, Execution, MemoryOperand, Nodes, OperationRef, Stack}
import com.tabishev.leshy.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Node}
import com.tabishev.leshy.runtime.{FrameOffset, Runtime}

import java.io.File
import java.util
import scala.util.Random

class BytecodeCompilerSpec extends munit.FunSuite {
  private def genOrigin(): Nodes.Origin = Nodes.Origin(null, OperationRef("", Random.nextInt()), null)
  private def finalNode(): Node = Nodes.Final(genOrigin())
  private def executeNode(size: Int, ex: Execution): Node = Nodes.Execute(genOrigin(), Nodes.Execute(genOrigin(), finalNode(), ex), Stack.SetSize(0, size))

  test("write4") {
    testExecution(4, Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("write8") {
    testExecution(8, Const.Write8(Long.MaxValue - 777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("setSize") {
    testExecution(0, Stack.SetSize(0, 12))
  }

  test("testFlagBranch") {
    testBranch(BranchExecution.Const(true))
    testBranch(BranchExecution.Const(false))
  }

  private def testExecution(size: Int, ex: Execution): Unit = check(executeNode(size, ex))

  private def testBranch(ex: BranchExecution): Unit = check(Nodes.Branch(
    genOrigin(),
    executeNode(4, Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero))),
    executeNode(4, Const.Write4(888, MemoryOperand.Stack(FrameOffset.Zero))),
    ex
  ))

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
