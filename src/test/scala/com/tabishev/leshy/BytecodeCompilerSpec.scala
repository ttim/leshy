package com.tabishev.leshy

import com.tabishev.leshy
import com.tabishev.leshy.lang.examples.Implementations
import com.tabishev.leshy.node.{BytecodeCompiler, Command, Condition, ConditionModifier, Executor, MemoryOperand, MemoryOperandOrBytes, Node}
import com.tabishev.leshy.runtime.{Bytes, FrameOffset, StackMemory}
import com.tabishev.leshy.node.Runners.MemoryOperandExtensions

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

    val prepare: StackMemory => Unit = stack => {
      stack.setFramesize(16)
      op.materialize(stack).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Command.Negate(4, dst, opArg(op)))
    testExecution(prepare, Command.Negate(8, dst, opArg(op)))
  }

  test("set") {
    val op = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(8))

    val prepare: StackMemory => Unit = stack => {
      stack.setFramesize(16)
      op.materialize(stack).putLong(Long.MaxValue - 777)
    }
    testExecution(prepare, Command.Set(4, dst, opArg(op)))
    testExecution(prepare, Command.Set(8, dst, opArg(op)))
  }

  test("sum") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(16))

    val prepare: StackMemory => Unit = stack => {
      stack.setFramesize(24)
      op1.materialize(stack).putLong(Long.MaxValue - 777)
      op2.materialize(stack).putLong(Long.MaxValue - 888)
    }

    testExecution(prepare, Command.Sum(4, dst, intArg(5), intArg(7)))
    testExecution(prepare, Command.Sum(4, dst, opArg(op1), intArg(7)))
    testExecution(prepare, Command.Sum(4, dst, opArg(op1), opArg(op2)))
    testExecution(prepare, Command.Sum(8, dst, longArg(5), longArg(7)))
    testExecution(prepare, Command.Sum(8, dst, opArg(op1), longArg(7)))
    testExecution(prepare, Command.Sum(8, dst, opArg(op1), longArg(7)))
  }

  test("mult") {
    val op1 = MemoryOperand.Stack(FrameOffset.nonNegative(0))
    val op2 = MemoryOperand.Stack(FrameOffset.nonNegative(8))
    val dst = MemoryOperand.Stack(FrameOffset.nonNegative(16))

    val prepare: StackMemory => Unit = stack => {
      stack.setFramesize(24)
      op1.materialize(stack).putLong(Long.MaxValue - 777)
      op2.materialize(stack).putLong(Long.MaxValue - 888)
    }

    testExecution(prepare, Command.Mult(4, dst, opArg(op1), intArg(7)))
    testExecution(prepare, Command.Mult(4, dst, opArg(op1), opArg(op2)))
    testExecution(prepare, Command.Mult(8, dst, opArg(op1), longArg(7)))
    testExecution(prepare, Command.Mult(8, dst, opArg(op1), longArg(7)))
  }

  test("setSize") {
    testExecution(_ => (), Command.SetFramesize(12))
  }

  test("test branch") {
    val intOp = MemoryOperandOrBytes.MemoryOperand(MemoryOperand.Stack(FrameOffset.nonNegative(0)))
    val longOp = MemoryOperandOrBytes.MemoryOperand(MemoryOperand.Stack(FrameOffset.nonNegative(0)))
    def test4(condition: Condition): Unit = testBranch(stack => stack.setFramesize(4), condition)
    def test8(condition: Condition): Unit = testBranch(stack => stack.setFramesize(8), condition)

    test4(Condition.Const(true))
    test4(Condition.Const(false))

    test4(Condition.Binary(4, intOp, ConditionModifier.LE, intArg(0)))
    test4(Condition.Binary(4, intOp, ConditionModifier.LE, intArg(-1)))
    test4(Condition.Binary(4, intOp, ConditionModifier.LE, intArg(1)))

    test4(Condition.Binary(4, intOp, ConditionModifier.GT, intArg(0)))
    test4(Condition.Binary(4, intOp, ConditionModifier.GT, intArg(-1)))
    test4(Condition.Binary(4, intOp, ConditionModifier.GT, intArg(1)))

    test8(Condition.Binary(8, longOp, ConditionModifier.LE, longArg(0)))
    test8(Condition.Binary(8, longOp, ConditionModifier.LE, longArg(-1)))
    test8(Condition.Binary(8, longOp, ConditionModifier.LE, longArg(1)))
  }

  def opArg(value: MemoryOperand): MemoryOperandOrBytes = MemoryOperandOrBytes.MemoryOperand(value)
  def intArg(value: Int): MemoryOperandOrBytes = MemoryOperandOrBytes.Bytes(Bytes.fromInt(value))
  def longArg(value: Long): MemoryOperandOrBytes = MemoryOperandOrBytes.Bytes(Bytes.fromLong(value))

  private def testExecution(prepare: StackMemory => Unit, command: Command): Unit =
    check(prepare, TestNodes.Run(command, TestNodes.Final()))

  private def testBranch(prepare: StackMemory => Unit, cond: Condition): Unit =
    check(prepare, TestNodes.Branch(
      cond,
      TestNodes.Run(Command.Set(4, MemoryOperand.Stack(FrameOffset.Zero), intArg(777)), TestNodes.Final()),
      TestNodes.Run(Command.Set(4, MemoryOperand.Stack(FrameOffset.Zero), intArg(888)), TestNodes.Final())
    ))

  private def check(prepare: StackMemory => Unit, node: Node): Unit = {
    val executor = new Executor()

    val expectedStack = new StackMemory()
    prepare(expectedStack)
    val expectedFinal = executor.run(node, expectedStack)
    val expected = expectedStack.currentStackFrame()

    val actualStack = new StackMemory()
    prepare(actualStack)
    val actualFinal = BytecodeCompiler.compile(executor, executor, node)(executor).runFully(actualStack).node
    val actual = actualStack.currentStackFrame()

    assert(expectedFinal == actualFinal)
    assert(util.Arrays.equals(expected, actual))
  }
}
