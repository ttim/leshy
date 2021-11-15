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

  final case class Gt4(op1: IntProvider, op2: IntProvider) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.get(runtime) > op2.get(runtime)

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(op1.expression, BranchModifier.GT, op2.expression, ifTrue)
  }

  final case class Gt8(op1: LongProvider, op2: LongProvider) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean = op1.get(runtime) > op2.get(runtime)

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(op1.expression, BranchModifier.GT, op2.expression, ifTrue)
  }

  final case class Le4(op1: IntProvider, op2: IntProvider) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.get(runtime) <= op2.get(runtime)

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(op1.expression, BranchModifier.LE, op2.expression, ifTrue)
  }

  final case class Le8(op1: LongProvider, op2: LongProvider) extends BranchExecution {
    override def execute(runtime: Runtime): Boolean =
      op1.get(runtime) <= op2.get(runtime)

    override def generate(writer: MethodVisitor, ifTrue: Label): Unit =
      writer.branch(op1.expression, BranchModifier.LE, op2.expression, ifTrue)
  }

  def gt4(op1: MemoryOperand | Int, op2: MemoryOperand | Int): BranchExecution =
    Gt4(IntProvider.create(op1), IntProvider.create(op2))

  def gt8(op1: MemoryOperand | Long, op2: MemoryOperand | Long): BranchExecution =
    Gt8(LongProvider.create(op1), LongProvider.create(op2))

  def le4(op1: MemoryOperand | Int, op2: MemoryOperand | Int): BranchExecution =
    Le4(IntProvider.create(op1), IntProvider.create(op2))

  def le8(op1: MemoryOperand | Long, op2: MemoryOperand | Long): BranchExecution =
    Le8(LongProvider.create(op1), LongProvider.create(op2))
}
