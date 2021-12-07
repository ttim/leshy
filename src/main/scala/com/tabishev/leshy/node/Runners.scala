package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{Bytes, MemoryRef, StackMemory}

extension (op: MemoryOperand) {
  def materialize(stack: StackMemory): MemoryRef = op match {
    case MemoryOperand.Stack(offset) => stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }
}

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
    case Condition.Gt(4, op1, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) > op2P.get(stack)
    case Condition.Gt(8, op1, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) > op2P.get(stack)
    case Condition.Le(4, op1, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) <= op2P.get(stack)
    case Condition.Le(8, op1, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) <= op2P.get(stack)
    case Condition.Eq(4, op1, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (stack: StackMemory) => op1P.get(stack) == op2P.get(stack)
    case Condition.Eq(8, op1, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (stack: StackMemory) => op1P.get(stack) == op2P.get(stack)
  }

  private def intOp(op: MemoryOperand | Bytes): IntProvider = op match {
    case op: MemoryOperand => IntProvider.Operand(op)
    case op: Bytes => IntProvider.Const(op.asInt)
  }

  private def longOp(op: MemoryOperand | Bytes): LongProvider = op match {
    case op: MemoryOperand => LongProvider.Operand(op)
    case op: Bytes => LongProvider.Const(op.asLong)
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

  def create(constOrOperand: Int | MemoryOperand): IntProvider = constOrOperand match {
    case const: Int => Const(const)
    case operand: MemoryOperand => Operand(operand)
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

  def create(constOrOperand: Long | MemoryOperand): LongProvider = constOrOperand match {
    case const: Long => Const(const)
    case operand: MemoryOperand => Operand(operand)
  }
}

// todo: write about premature optimization: there've been 0 reason to optimize either for interpreter or compiler nodes...
