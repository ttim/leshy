package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{BranchExecution, Const, Execution, MemoryOperand, Mult, Negate, Nodes, OperationRef, Stack, Sum, Set}
import com.tabishev.leshy.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Node}
import com.tabishev.leshy.runtime.{FrameOffset, Runtime, StackMemory}

import java.io.File
import java.util
import scala.util.Random

class BytecodeCompilerSpec extends munit.FunSuite {
  private def genOrigin(): Nodes.Origin = Nodes.Origin(null, OperationRef("", Random.nextInt()), null)
  private def finalNode(): Node = Nodes.Final(genOrigin())
  private def executeNode(ex: Execution*): Node = ex.toList match {
    case head :: tail => Nodes.Execute(genOrigin(), executeNode(tail:_*), head)
    case nil => finalNode()
  }

  test("const") {
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(8)
    }

    testExecution(prepare, Const.Write4(777, dst))
    testExecution(prepare, Const.Write8(Long.MaxValue - 777, dst))
  }

  test("negate") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Negate.M4(op, dst))
    testExecution(prepare, Negate.M8(op, dst))
  }

  test("set") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Set.M4(op, dst))
    testExecution(prepare, Set.M8(op, dst))
  }

  test("sum") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(16))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(24)
      op1.materialize(runtime).putLong(Long.MaxValue - 777)
      op2.materialize(runtime).putLong(Long.MaxValue - 888)
    }

    testExecution(prepare, Sum.MC4(op1, 7, dst))
    testExecution(prepare, Sum.MM4(op1, op2, dst))
    testExecution(prepare, Sum.MC8(op1, 7, dst))
    testExecution(prepare, Sum.MC8(op1, 7, dst))
  }

  test("mult") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(16))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(24)
      op1.materialize(runtime).putLong(Long.MaxValue - 777)
      op2.materialize(runtime).putLong(Long.MaxValue - 888)
    }

    testExecution(prepare, Mult.MC4(op1, 7, dst))
    testExecution(prepare, Mult.MM4(op1, op2, dst))
    testExecution(prepare, Mult.MC8(op1, 7, dst))
    testExecution(prepare, Mult.MC8(op1, 7, dst))
  }

  test("setSize") {
    testExecution(_ => (), Stack.SetSize(0, 12))
  }

  test("test branch") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    def test4(branch: BranchExecution): Unit = testBranch(runtime => runtime.stack.setFramesize(4), branch)
    def test8(branch: BranchExecution): Unit = testBranch(runtime => runtime.stack.setFramesize(8), branch)

    test4(BranchExecution.Const(true))
    test4(BranchExecution.Const(false))

    test4(BranchExecution.le4(op, 0))
    test4(BranchExecution.le4(op, -1))
    test4(BranchExecution.le4(op, 1))

    test4(BranchExecution.gt4(op, 0))
    test4(BranchExecution.gt4(op, -1))
    test4(BranchExecution.gt4(op, 1))

    test8(BranchExecution.le8(op, 0))
    test8(BranchExecution.le8(op, -1))
    test8(BranchExecution.le8(op, 1))
  }

  private def testExecution(prepare: Runtime => Unit, ex: Execution*): Unit = check(prepare, executeNode(ex:_*))

  private def testBranch(prepare: Runtime => Unit, ex: BranchExecution): Unit = check(prepare, Nodes.Branch(
    genOrigin(),
    executeNode(Const.Write4(777, MemoryOperand.Stack(FrameOffset.Zero))),
    executeNode(Const.Write4(888, MemoryOperand.Stack(FrameOffset.Zero))),
    ex
  ))

  private def check(prepare: Runtime => Unit, node: Node): Unit = {
    val expectedRuntime = new Runtime()
    prepare(expectedRuntime)
    val expectedFinal = node.run(expectedRuntime)
    val expected = expectedRuntime.stack.currentStackFrame()

    val actualRuntime = new Runtime()
    prepare(actualRuntime)
    val actualFinal = BytecodeCompiler.compile(node).run(actualRuntime)
    val actual = actualRuntime.stack.currentStackFrame()

    assert(expectedFinal == actualFinal)
    assert(util.Arrays.equals(expected, actual))
  }
}
