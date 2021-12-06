package com.tabishev.leshy.node

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, StackMemory}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import com.tabishev.leshy.bytecode.{BranchModifier, BytecodeExpression, branch, statement}
import com.tabishev.leshy.bytecode.BytecodeExpression.*

object Generate {
  def command(command: Command, writer: MethodVisitor): Unit = command match {
    case Command.Noop =>
      ()
    case Command.SetFramesize(size) =>
      writer.statement(invokeVirtual(classOf[StackMemory], "setFramesize", BytecodeCompiler.StackExpression, const(size)))
    case Command.Sum(4, dst, op1, op2) =>
      writer.statement(dst.putInt(sum(intOp(op1), intOp(op2))))
    case Command.Sum(8, dst, op1, op2) =>
      writer.statement(dst.putLong(sum(longOp(op1), longOp(op2))))
    case Command.Mult(4, dst, op1, op2) =>
      writer.statement(dst.putInt(mult(intOp(op1), intOp(op2))))
    case Command.Mult(8, dst, op1, op2) =>
      writer.statement(dst.putLong(mult(longOp(op1), longOp(op2))))
    case Command.Negate(4, dst, op) =>
      writer.statement(dst.putInt(negate(intOp(op))))
    case Command.Negate(8, dst, op) =>
      writer.statement(dst.putLong(negate(longOp(op))))
    case Command.Set(4, dst, op) =>
      writer.statement(dst.putInt(intOp(op)))
    case Command.Set(8, dst, op) =>
      writer.statement(dst.putLong(longOp(op)))
  }

  def condition(condition: Condition, writer: MethodVisitor, ifTrue: Label): Unit = condition match {
    case Condition.Const(flag) =>
      if (flag) writer.visitJumpInsn(Opcodes.GOTO, ifTrue) // else do nothing
    case Condition.Gt(4, op1, op2) =>
      writer.branch(intOp(op1), BranchModifier.GT, intOp(op2), ifTrue)
    case Condition.Gt(8, op1, op2) =>
      writer.branch(longOp(op1), BranchModifier.GT, longOp(op2), ifTrue)
    case Condition.Le(4, op1, op2) =>
      writer.branch(intOp(op1), BranchModifier.LE, intOp(op2), ifTrue)
    case Condition.Le(8, op1, op2) =>
      writer.branch(longOp(op1), BranchModifier.LE, longOp(op2), ifTrue)
  }

  private def intOp(op: MemoryOperand | Bytes): BytecodeExpression = op match {
    case op: MemoryOperand => invokeVirtual(classOf[MemoryRef], "getInt", op.expression)
    case op: Bytes => const(op.asInt)
  }

  private def longOp(op: MemoryOperand | Bytes): BytecodeExpression = op match {
    case op: MemoryOperand => invokeVirtual(classOf[MemoryRef], "getLong", op.expression)
    case op: Bytes => const(op.asLong)
  }
}

extension (op: MemoryOperand) {
  def expression: BytecodeExpression = op match {
    case MemoryOperand.Stack(offset) =>
      val frameOffset = invokeStatic(classOf[FrameOffset], "nonNegative", const(offset.get))
      invokeVirtual(classOf[StackMemory], "getRef", BytecodeCompiler.StackExpression, frameOffset)
    case MemoryOperand.Native(offset) =>
      ???
  }

  def putInt(value: BytecodeExpression): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "putInt", expression, value)

  def putLong(value: BytecodeExpression): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "putLong", expression, value)
}
