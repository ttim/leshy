package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{BranchExecution, Const, Execution, MemoryOperand, Nodes, OperationRef, Stack}
import com.tabishev.leshy.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Node}
import com.tabishev.leshy.runtime.{FrameOffset, Runtime, StackMemory}

import java.io.File
import java.util
import scala.util.Random

class BytecodeCompilerSpec extends munit.FunSuite {
  private val prepareNothing: StackMemory => Unit = { _ => () }

  private def prepareSize(size: Int): StackMemory => Unit = { stack => stack.setFramesize(size) }

  private def genOrigin(): Nodes.Origin = Nodes.Origin(null, OperationRef("", Random.nextInt()), null)
  private def finalNode(): Node = Nodes.Final(genOrigin())
  private def executeNode(ex: Execution): Node = Nodes.Execute(genOrigin(), finalNode(), ex)

  test("write4") {
    testExecution(prepareSize(4), Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("write8") {
    testExecution(prepareSize(4), Const.Write8(Long.MaxValue - 777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("setSize") {
    testExecution(prepareNothing, Stack.SetSize(0, 12))
  }

  test("testFlagBranch") {
    testBranch(prepareSize(4), BranchExecution.Const(true))
    testBranch(prepareSize(4), BranchExecution.Const(false))
  }

  private def testExecution(prepare: StackMemory => Unit, ex: Execution): Unit = check(prepare, executeNode(ex))

  private def testBranch(prepare: StackMemory => Unit, ex: BranchExecution): Unit = check(prepare, Nodes.Branch(
    genOrigin(),
    executeNode(Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero))),
    executeNode(Const.Write4(888, MemoryOperand.Stack(FrameOffset.Zero))),
    ex
  ))

  private def check(prepare: StackMemory => Unit, node: Node): Unit = {
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
