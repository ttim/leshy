package com.tabishev.leshy.node

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.{MemoryRef, Runtime}

extension (op: MemoryOperand) {
  def materialize(runtime: Runtime): MemoryRef = op match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }
}

object Runners {
  def command(command: Command): CommandImpl = command match {
    case Command.Noop =>
      (runtime: Runtime) => ()
    case Command.SetFramesize(size) =>
      (runtime: Runtime) => runtime.stack.setFramesize(size)
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
      (runtime: Runtime) => flag
    case Condition.Gt(4, op1, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (runtime: Runtime) => op1P.get(runtime) > op2P.get(runtime)
    case Condition.Gt(8, op1, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (runtime: Runtime) => op1P.get(runtime) > op2P.get(runtime)
    case Condition.Le(4, op1, op2) =>
      val (op1P, op2P) = (intOp(op1), intOp(op2))
      (runtime: Runtime) => op1P.get(runtime) <= op2P.get(runtime)
    case Condition.Le(8, op1, op2) =>
      val (op1P, op2P) = (longOp(op1), longOp(op2))
      (runtime: Runtime) => op1P.get(runtime) <= op2P.get(runtime)
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

  final def run(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(op1.get(runtime), op2.get(runtime)))
}

abstract class BinaryLongImpl(val dst: MemoryOperand, val op1: LongProvider, val op2: LongProvider) extends CommandImpl {
  def eval(op1: Long, op2: Long): Long

  final def run(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(op1.get(runtime), op2.get(runtime)))
}

abstract class UnaryIntImpl(val dst: MemoryOperand, val op: IntProvider) extends CommandImpl {
  def eval(op: Int): Int

  final def run(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(op.get(runtime)))
}

abstract class UnaryLongImpl(val dst: MemoryOperand, val op: LongProvider) extends CommandImpl {
  def eval(op: Long): Long

  final def run(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(op.get(runtime)))
}


sealed abstract class IntProvider {
  def get(runtime: Runtime): Int
}

object IntProvider {
  final case class Const(value: Int) extends IntProvider {
    override def get(runtime: Runtime): Int = value
  }

  final case class Operand(value: MemoryOperand) extends IntProvider {
    override def get(runtime: Runtime): Int = value.materialize(runtime).getInt()
  }

  def create(constOrOperand: Int | MemoryOperand): IntProvider = constOrOperand match {
    case const: Int => Const(const)
    case operand: MemoryOperand => Operand(operand)
  }
}

sealed abstract class LongProvider {
  def get(runtime: Runtime): Long
}

object LongProvider {
  final case class Const(value: Long) extends LongProvider {
    override def get(runtime: Runtime): Long = value
  }

  final case class Operand(value: MemoryOperand) extends LongProvider {
    override def get(runtime: Runtime): Long = value.materialize(runtime).getLong()
  }

  def create(constOrOperand: Long | MemoryOperand): LongProvider = constOrOperand match {
    case const: Long => Const(const)
    case operand: MemoryOperand => Operand(operand)
  }
}

// todo: write about premature optimization: there've been 0 reason to optimize either for interpreter or compiler nodes...
