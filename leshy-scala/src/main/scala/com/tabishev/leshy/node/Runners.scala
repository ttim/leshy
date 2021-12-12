package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{Bytes, MemoryRef, StackMemory}
import com.tabishev.leshy.node.Runners.MemoryOperandExtensions

object Runners {
  def command(command: Command): CommandImpl = command match {
    case Command.Noop =>
      (stack: StackMemory) => ()
    case Command.SetFramesize(size) =>
      (stack: StackMemory) => stack.setFramesize(size)
    case Command.Sum(4, dst, op1, op2) =>
      new BinaryIntImpl(dst, intOp(op1), intOp(op2)) {
        override def eval(op1: Int, op2: Int): Int = op1 + op2
      }
    case Command.Sum(8, dst, op1, op2) =>
      new BinaryLongImpl(dst, longOp(op1), longOp(op2)) {
        override def eval(op1: Long, op2: Long): Long = op1 + op2
      }
    case Command.Mult(4, dst, op1, op2) =>
      new BinaryIntImpl(dst, intOp(op1), intOp(op2)) {
        override def eval(op1: Int, op2: Int): Int = op1 * op2
      }
    case Command.Mult(8, dst, op1, op2) =>
      new BinaryLongImpl(dst, longOp(op1), longOp(op2)) {
        override def eval(op1: Long, op2: Long): Long = op1 * op2
      }
    case Command.Negate(4, dst, op) =>
      new UnaryIntImpl(dst, intOp(op)) {
        override def eval(op: Int): Int = -op
      }
    case Command.Negate(8, dst, op) =>
      new UnaryLongImpl(dst, longOp(op)) {
        override def eval(op: Long): Long = -op
      }
    case Command.Set(4, dst, op) =>
      new UnaryIntImpl(dst, intOp(op)) {
        override def eval(op: Int): Int = op
      }
    case Command.Set(8, dst, op) =>
      new UnaryLongImpl(dst, longOp(op)) {
        override def eval(op: Long): Long = op
      }
  }

  def condition(condition: Condition): ConditionImpl = condition match {
    case Condition.Const(flag) =>
      (stack: StackMemory) => flag
    case Condition.Binary(4, op1, ConditionModifier.GT, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) > op2P.get(stack)
    case Condition.Binary(8, op1, ConditionModifier.GT, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) > op2P.get(stack)
    case Condition.Binary(4, op1, ConditionModifier.LE, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) <= op2P.get(stack)
    case Condition.Binary(8, op1, ConditionModifier.LE, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) <= op2P.get(stack)
    case Condition.Binary(4, op1, ConditionModifier.EQ, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) == op2P.get(stack)
    case Condition.Binary(8, op1, ConditionModifier.EQ, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) == op2P.get(stack)
  }

  private def intOp(bytesOrOp: Either[Bytes, MemoryOperand]): IntProvider = bytesOrOp match {
    case Left(bytes) => IntProvider.Const(bytes.asInt)
    case Right(op) => IntProvider.Operand(op)
  }

  private def longOp(bytesOrOp: Either[Bytes, MemoryOperand]): LongProvider = bytesOrOp match {
    case Left(bytes) => LongProvider.Const(bytes.asLong)
    case Right(op) => LongProvider.Operand(op)
  }

  implicit class MemoryOperandExtensions(op: MemoryOperand) {
    def materialize(stack: StackMemory): MemoryRef = op match {
      case MemoryOperand.Stack(offset) => stack.getRef(offset)
      case MemoryOperand.Native(offset) => ???
    }
  }
}

abstract class BinaryIntImpl(val dst: MemoryOperand, val op1: IntProvider, val op2: IntProvider) extends CommandImpl {
  def eval(op1: Int, op2: Int): Int

  final def run(stack: StackMemory): Unit =
    dst.materialize(stack).putInt(eval(op1.get(stack), op2.get(stack)))
}

abstract class BinaryLongImpl(val dst: MemoryOperand, val op1: LongProvider, val op2: LongProvider) extends CommandImpl {
  def eval(op1: Long, op2: Long): Long

  final def run(stack: StackMemory): Unit =
    dst.materialize(stack).putLong(eval(op1.get(stack), op2.get(stack)))
}

abstract class UnaryIntImpl(val dst: MemoryOperand, val op: IntProvider) extends CommandImpl {
  def eval(op: Int): Int

  final def run(stack: StackMemory): Unit =
    dst.materialize(stack).putInt(eval(op.get(stack)))
}

abstract class UnaryLongImpl(val dst: MemoryOperand, val op: LongProvider) extends CommandImpl {
  def eval(op: Long): Long

  final def run(stack: StackMemory): Unit =
    dst.materialize(stack).putLong(eval(op.get(stack)))
}

sealed abstract class IntProvider {
  def get(stack: StackMemory): Int
}

object IntProvider {
  final case class Const(value: Int) extends IntProvider {
    override def get(stack: StackMemory): Int = value
  }

  final case class Operand(value: MemoryOperand) extends IntProvider {
    override def get(stack: StackMemory): Int = value.materialize(stack).getInt()
  }
}

sealed abstract class LongProvider {
  def get(stack: StackMemory): Long
}

object LongProvider {
  final case class Const(value: Long) extends LongProvider {
    override def get(stack: StackMemory): Long = value
  }

  final case class Operand(value: MemoryOperand) extends LongProvider {
    override def get(stack: StackMemory): Long = value.materialize(stack).getLong()
  }
}

// todo: write about premature optimization: there've been 0 reason to optimize either for interpreter or compiler nodes...
