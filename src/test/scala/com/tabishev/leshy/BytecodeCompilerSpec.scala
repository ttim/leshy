package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.compiler.{BranchExecution, Execution, IntProvider, LongProvider, MemoryOperand, Mult, Negate, Nodes, OperationRef, Set, Stack, Sum}
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

  test("negate") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Negate.Length4(dst, IntProvider.create(op)))
    testExecution(prepare, Negate.Length8(dst, LongProvider.create(op)))
  }

  test("set") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Set.Length4(dst, IntProvider.create(op)))
    testExecution(prepare, Set.Length8(dst, LongProvider.create(op)))
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

    testExecution(prepare, Sum.Length4(dst, IntProvider.create(5), IntProvider.create(7)))
    testExecution(prepare, Sum.Length4(dst, IntProvider.create(op1), IntProvider.create(7)))
    testExecution(prepare, Sum.Length4(dst, IntProvider.create(op1), IntProvider.create(op2)))
    testExecution(prepare, Sum.Length8(dst, LongProvider.create(5), LongProvider.create(7)))
    testExecution(prepare, Sum.Length8(dst, LongProvider.create(op1), LongProvider.create(7)))
    testExecution(prepare, Sum.Length8(dst, LongProvider.create(op1), LongProvider.create(7)))
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

    testExecution(prepare, Mult.Length4(dst, IntProvider.create(op1), IntProvider.create(7)))
    testExecution(prepare, Mult.Length4(dst, IntProvider.create(op1), IntProvider.create(op2)))
    testExecution(prepare, Mult.Length8(dst, LongProvider.create(op1), LongProvider.create(7)))
    testExecution(prepare, Mult.Length8(dst, LongProvider.create(op1), LongProvider.create(7)))
  }

  test("setSize") {
    testExecution(_ => (), Stack.SetSize(12))
  }

  test("test branch") {
    val intOp = IntProvider.create(MemoryOperand.Stack(FrameOffset.nonNegative(0)))
    val longOp = LongProvider.create(MemoryOperand.Stack(FrameOffset.nonNegative(0)))
    def test4(branch: BranchExecution): Unit = testBranch(runtime => runtime.stack.setFramesize(4), branch)
    def test8(branch: BranchExecution): Unit = testBranch(runtime => runtime.stack.setFramesize(8), branch)

    test4(BranchExecution.Const(true))
    test4(BranchExecution.Const(false))

    test4(BranchExecution.Le4(intOp, IntProvider.create(0)))
    test4(BranchExecution.Le4(intOp, IntProvider.create(-1)))
    test4(BranchExecution.Le4(intOp, IntProvider.create(1)))

    test4(BranchExecution.Gt4(intOp, IntProvider.create(0)))
    test4(BranchExecution.Gt4(intOp, IntProvider.create(-1)))
    test4(BranchExecution.Gt4(intOp, IntProvider.create(1)))

    test8(BranchExecution.Le8(longOp, LongProvider.create(0)))
    test8(BranchExecution.Le8(longOp, LongProvider.create(-1)))
    test8(BranchExecution.Le8(longOp, LongProvider.create(1)))
  }

  private def testExecution(prepare: Runtime => Unit, ex: Execution*): Unit = check(prepare, executeNode(ex:_*))

  private def testBranch(prepare: Runtime => Unit, ex: BranchExecution): Unit = check(prepare, Nodes.Branch(
    genOrigin(),
    executeNode(Set.Length4(MemoryOperand.Stack(FrameOffset.Zero), IntProvider.create(777))),
    executeNode(Set.Length4(MemoryOperand.Stack(FrameOffset.Zero), IntProvider.create(888))),
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
