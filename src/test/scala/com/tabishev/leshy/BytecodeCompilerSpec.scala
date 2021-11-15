package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{BranchExecution, Const, Execution, MemoryOperand, Negate, Nodes, OperationRef, Stack, Sum}
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
  private def executeNode(ex: Execution*): Node = ex.toList match {
    case head :: tail => Nodes.Execute(genOrigin(), executeNode(tail:_*), head)
    case nil => finalNode()
  }

  test("write4") {
    testExecution(prepareSize(4), Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("write8") {
    testExecution(prepareSize(4), Const.Write8(Long.MaxValue - 777, MemoryOperand.Stack(FrameOffset.Zero)))
  }

  test("negate") {
    val op = MemoryOperand.Stack(FrameOffset.Zero)
    testExecution(prepareSize(4), Const.Write4(1, op), Negate.M4(op, op))
    testExecution(prepareSize(8), Const.Write8(1, op), Negate.M8(op, op))
  }

  test("sum4") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(4))
    val op3 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val op4 = MemoryOperand.Stack(FrameOffset.nonNegative(12))

    testExecution(prepareSize(16),
      Const.Write4(1, op1),
      Const.Write4(10, op2),
      Sum.MC4(op1, 7, op3),
      Sum.MM4(op1, op2, op4)
    )
  }

  test("sum8") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val op3 = MemoryOperand.Stack(FrameOffset.nonNegative(16))
    val op4 = MemoryOperand.Stack(FrameOffset.nonNegative(24))

    testExecution(prepareSize(32),
      Const.Write8(1, op1),
      Const.Write8(10, op2),
      Sum.MC8(op1, 7, op3),
      Sum.MM8(op1, op2, op4)
    )
  }

  test("setSize") {
    testExecution(prepareNothing, Stack.SetSize(0, 12))
  }

  test("testFlagBranch") {
    testBranch(prepareSize(4), BranchExecution.Const(true))
    testBranch(prepareSize(4), BranchExecution.Const(false))
  }

  test("test LessOrEqual branch") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    testBranch(prepareSize(4), BranchExecution.LessOrEqualMC4(op, 0))
    testBranch(prepareSize(4), BranchExecution.LessOrEqualMC4(op, -1))
    testBranch(prepareSize(4), BranchExecution.LessOrEqualMC4(op, 1))
  }

  private def testExecution(prepare: StackMemory => Unit, ex: Execution*): Unit = check(prepare, executeNode(ex:_*))

  private def testBranch(prepare: StackMemory => Unit, ex: BranchExecution): Unit = check(prepare, Nodes.Branch(
    genOrigin(),
    executeNode(Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero))),
    executeNode(Const.Write4(888, MemoryOperand.Stack(FrameOffset.Zero))),
    ex
  ))

  private def check(prepare: StackMemory => Unit, node: Node): Unit = {
    val expectedRuntime = new Runtime()
    prepare(expectedRuntime.stack)
    val expectedFinal = node.run(expectedRuntime)
    val expected = expectedRuntime.stack.currentStackFrame()

    val actualRuntime = new Runtime()
    prepare(actualRuntime.stack)
    val actualFinal = BytecodeCompiler.compile(node).run(actualRuntime)
    val actual = actualRuntime.stack.currentStackFrame()

    assert(expectedFinal == actualFinal)
    assert(util.Arrays.equals(expected, actual))
  }
}
