package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.Runtime
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import com.tabishev.leshy.bytecode.*
import com.tabishev.leshy.bytecode.BytecodeExpression._

abstract class BranchExecution {
  def execute(runtime: Runtime): Boolean
  def generate(writer: MethodVisitor, ifTrue: Label): Unit
}

object BranchExecution {
  final case class Const(value: Boolean) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = value
    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      if (value) writer.visitJumpInsn(Opcodes.GOTO, ifTrue) // else do nothing
  }

  final case class GtMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() > op2.materialize(runtime).getInt()

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getInt(op1), BranchModifier.GT, MemoryOps.getInt(op2), ifTrue)
  }

  final case class GtMC4(op1: MemoryOperand, op2: Int) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() > op2

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getInt(op1), BranchModifier.GT, const(op2), ifTrue)
  }

  final case class GtMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() > op2.materialize(runtime).getLong()

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getLong(op1), BranchModifier.GT, MemoryOps.getLong(op2), ifTrue)
  }

  final case class GtMC8(op1: MemoryOperand, op2: Long) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() > op2

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getLong(op1), BranchModifier.GT, const(op2), ifTrue)
  }

  final case class LeMM4(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getInt() <= op2.materialize(runtime).getInt()

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getInt(op1), BranchModifier.LE, MemoryOps.getInt(op2), ifTrue)
  }

  final case class LeMC4(op1: MemoryOperand, op2: Int) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getInt() <= op2

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getInt(op1), BranchModifier.LE, const(op2), ifTrue)
  }

  final case class LeMM8(op1: MemoryOperand, op2: MemoryOperand) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.materialize(runtime).getLong() <= op2.materialize(runtime).getLong()

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getInt(op1), BranchModifier.LE, MemoryOps.getInt(op2), ifTrue)
  }

  final case class LeMC8(op1: MemoryOperand, op2: Long) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.materialize(runtime).getLong() <= op2

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(MemoryOps.getLong(op1), BranchModifier.LE, const(op2), ifTrue)
  }

  def gt4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => GtMM4(op1, op2)
      case (op1: MemoryOperand, op2: Int) => GtMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => LeMC4(op2, op1)
      case (op1: Int, op2: Int) => Const(op1 > op2)
    }

  def gt8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => GtMM8(op1, op2)
      case (op1: MemoryOperand, op2: Long) => GtMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => LeMC8(op2, op1)
      case (op1: Long, op2: Long) => Const(op1 > op2)
    }

  def le4(op1Union: MemoryOperand | Int, op2Union: MemoryOperand | Int): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => GtMM4(op2, op1)
      case (op1: MemoryOperand, op2: Int) => LeMC4(op1, op2)
      case (op1: Int, op2: MemoryOperand) => GtMC4(op2, op1)
      case (op1: Int, op2: Int) => Const(op1 <= op2)
    }

  def le8(op1Union: MemoryOperand | Long, op2Union: MemoryOperand | Long): BranchExecution =
    (op1Union, op2Union) match {
      case (op1: MemoryOperand, op2: MemoryOperand) => GtMM8(op2, op1)
      case (op1: MemoryOperand, op2: Long) => LeMC8(op1, op2)
      case (op1: Long, op2: MemoryOperand) => GtMC8(op2, op1)
      case (op1: Long, op2: Long) => Const(op1 <= op2)
    }
}
