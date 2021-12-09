package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, StackMemory}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import com.tabishev.leshy.bytecode.{BranchModifier, Expression, InvokeMethod, Statement}
import Expression.*
import com.tabishev.leshy.bytecode.Statement.*
import com.tabishev.leshy.runtime.Bytes
import com.tabishev.leshy.bytecode.InvokeMethod.invokeVirtual

object Generate {
  def command(command: Command, writer: MethodVisitor): Unit = {
    val expression = command match {
      case Command.Noop =>
        Expression.voidExpression()
      case Command.SetFramesize(size) =>
        invokeVirtual(classOf[StackMemory], "setFramesize", BytecodeCompiler.StackExpression, constExpression(size))
      case Command.Sum(4, dst, op1, op2) =>
        dst.putInt(sum(intOp(op1), intOp(op2)))
      case Command.Sum(8, dst, op1, op2) =>
        dst.putLong(sum(longOp(op1), longOp(op2)))
      case Command.Mult(4, dst, op1, op2) =>
        dst.putInt(mult(intOp(op1), intOp(op2)))
      case Command.Mult(8, dst, op1, op2) =>
        dst.putLong(mult(longOp(op1), longOp(op2)))
      case Command.Negate(4, dst, op) =>
        dst.putInt(negate(intOp(op)))
      case Command.Negate(8, dst, op) =>
        dst.putLong(negate(longOp(op)))
      case Command.Set(4, dst, op) =>
        dst.putInt(intOp(op))
      case Command.Set(8, dst, op) =>
        dst.putLong(longOp(op))
    }
    Statement.expression(expression).write(writer)
  }

  def condition(condition: Condition, writer: MethodVisitor, ifTrue: Label): Unit = condition match {
    case Condition.Const(flag) =>
      if (flag) writer.visitJumpInsn(Opcodes.GOTO, ifTrue) // else do nothing
    case Condition.Binary(4, op1, modifier, op2) =>
      branch(intOp(op1), branchModifier(modifier), intOp(op2), ifTrue).write(writer)
    case Condition.Binary(8, op1, modifier, op2) =>
      branch(longOp(op1), branchModifier(modifier), longOp(op2), ifTrue).write(writer)
  }

  private def branchModifier(conditionModifier: ConditionModifier): BranchModifier = conditionModifier match {
    case ConditionModifier.GT => BranchModifier.GT
    case ConditionModifier.LE => BranchModifier.LE
    case ConditionModifier.EQ => BranchModifier.EQ
  }

  private def intOp(op: MemoryOperand | Bytes): Expression = op match {
    case op: MemoryOperand => invokeVirtual(classOf[MemoryRef], "getInt", op.expression)
    case op: Bytes => constExpression(op.asInt)
  }

  private def longOp(op: MemoryOperand | Bytes): Expression = op match {
    case op: MemoryOperand => invokeVirtual(classOf[MemoryRef], "getLong", op.expression)
    case op: Bytes => constExpression(op.asLong)
  }
}

extension (op: MemoryOperand) {
  def expression: Expression = op match {
    case MemoryOperand.Stack(offset) =>
      val frameOffset = InvokeMethod.invokeStatic(classOf[FrameOffset], "nonNegative", constExpression(offset.get))
      invokeVirtual(classOf[StackMemory], "getRef", BytecodeCompiler.StackExpression, frameOffset)
    case MemoryOperand.Native(offset) =>
      ???
  }

  def putInt(value: Expression): Expression =
    invokeVirtual(classOf[MemoryRef], "putInt", expression, value)

  def putLong(value: Expression): Expression =
    invokeVirtual(classOf[MemoryRef], "putLong", expression, value)
}
