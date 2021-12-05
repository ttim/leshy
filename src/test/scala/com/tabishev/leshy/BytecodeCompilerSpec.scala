package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.compiler.*
import com.tabishev.leshy.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Command, Condition, Executor, Node}
import com.tabishev.leshy.runtime.{FrameOffset, Runtime, StackMemory, Symbols}
import com.tabishev.leshy.node.MemoryOperand
import com.tabishev.leshy.node.materialize

import java.io.File
import java.util
import scala.util.Random

object TestNodes {
  case class Final(id: Int = Random.nextInt()) extends Node.Final
  case class Run(command: Command, next: Node) extends Node.Run
  case class Branch(condition: Condition, ifTrue: Node, ifFalse: Node) extends Node.Branch
}

class BytecodeCompilerSpec extends munit.FunSuite {
  test("negate") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Command.Negate(4, dst, op))
    testExecution(prepare, Command.Negate(8, dst, op))
  }

  test("set") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: Runtime => Unit = runtime => {
      runtime.stack.setFramesize(16)
      op.materialize(runtime).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Command.Set(4, dst, op))
    testExecution(prepare, Command.Set(8, dst, op))
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

    testExecution(prepare, Command.Sum(4, dst, Bytes.fromInt(5), Bytes.fromInt(7)))
    testExecution(prepare, Command.Sum(4, dst, op1, Bytes.fromInt(7)))
    testExecution(prepare, Command.Sum(4, dst, op1, op2))
    testExecution(prepare, Command.Sum(8, dst, Bytes.fromLong(5), Bytes.fromLong(7)))
    testExecution(prepare, Command.Sum(8, dst, op1, Bytes.fromLong(7)))
    testExecution(prepare, Command.Sum(8, dst, op1, Bytes.fromLong(7)))
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

    testExecution(prepare, Command.Mult(4, dst, op1, Bytes.fromInt(7)))
    testExecution(prepare, Command.Mult(4, dst, op1, op2))
    testExecution(prepare, Command.Mult(8, dst, op1, Bytes.fromLong(7)))
    testExecution(prepare, Command.Mult(8, dst, op1, Bytes.fromLong(7)))
  }

  test("setSize") {
    testExecution(_ => (), Command.SetFramesize(12))
  }

  test("test branch") {
    val intOp = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val longOp = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    def test4(condition: Condition): Unit = testBranch(runtime => runtime.stack.setFramesize(4), condition)
    def test8(condition: Condition): Unit = testBranch(runtime => runtime.stack.setFramesize(8), condition)

    test4(Condition.Const(true))
    test4(Condition.Const(false))

    test4(Condition.Le(4, intOp, Bytes.fromInt(0)))
    test4(Condition.Le(4, intOp, Bytes.fromInt(-1)))
    test4(Condition.Le(4, intOp, Bytes.fromInt(1)))

    test4(Condition.Gt(4, intOp, Bytes.fromInt(0)))
    test4(Condition.Gt(4, intOp, Bytes.fromInt(-1)))
    test4(Condition.Gt(4, intOp, Bytes.fromInt(1)))

    test8(Condition.Le(8, longOp, Bytes.fromLong(0)))
    test8(Condition.Le(8, longOp, Bytes.fromLong(-1)))
    test8(Condition.Le(8, longOp, Bytes.fromLong(1)))
  }

  private def testExecution(prepare: Runtime => Unit, command: Command): Unit =
    check(prepare, TestNodes.Run(command, TestNodes.Final()))

  private def testBranch(prepare: Runtime => Unit, cond: Condition): Unit =
    check(prepare, TestNodes.Branch(
      cond,
      TestNodes.Run(Command.Set(4, MemoryOperand.Stack(FrameOffset.Zero), Bytes.fromInt(777)), TestNodes.Final()),
      TestNodes.Run(Command.Set(4, MemoryOperand.Stack(FrameOffset.Zero), Bytes.fromInt(888)), TestNodes.Final())
    ))

  private def check(prepare: Runtime => Unit, node: Node): Unit = {
    val executor = new Executor()

    val expectedRuntime = new Runtime()
    prepare(expectedRuntime)
    val expectedFinal = executor.run(node, expectedRuntime)
    val expected = expectedRuntime.stack.currentStackFrame()

    val actualRuntime = new Runtime()
    prepare(actualRuntime)
    val actualFinal = BytecodeCompiler.compile(executor, executor, node).runFully(actualRuntime).node
    val actual = actualRuntime.stack.currentStackFrame()

    assert(expectedFinal == actualFinal)
    assert(util.Arrays.equals(expected, actual))
  }
}
