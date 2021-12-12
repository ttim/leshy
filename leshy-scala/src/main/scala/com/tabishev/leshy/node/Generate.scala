package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, StackMemory}
import org.objectweb.asm.{Label, MethodVisitor}
import com.tabishev.leshy.bytecode.{BranchModifier, Expression}
import com.tabishev.leshy.bytecode.Expression._
import com.tabishev.leshy.bytecode.Statement._
import com.tabishev.leshy.bytecode.Statement.WriterExtension
import com.tabishev.leshy.runtime.Bytes

object Generate {
  def command(command: Command, writer: MethodVisitor): Unit = {
    val expr = command match {
      case Command.Noop =>
        Expression.void()
      case Command.SetFramesize(size) =>
        invokeVirtual(classOf[StackMemory], "setFramesize", BytecodeCompiler.StackExpression, const(size))
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
    writer.write(Expr(expr))
  }

  def condition(condition: Condition, writer: MethodVisitor, ifTrue: Label): Unit = condition match {
    case Condition.Const(flag) =>
      if (flag) writer.write(Goto(ifTrue)) // else do nothing
    case Condition.Binary(4, op1, modifier, op2) =>
      writer.write(Branch(intOp(op1), branchModifier(modifier), intOp(op2), ifTrue))
    case Condition.Binary(8, op1, modifier, op2) =>
      writer.write(Branch(longOp(op1), branchModifier(modifier), longOp(op2), ifTrue))
  }

  private def branchModifier(conditionModifier: ConditionModifier): BranchModifier = conditionModifier match {
    case ConditionModifier.GT => BranchModifier.GT
    case ConditionModifier.LE => BranchModifier.LE
    case ConditionModifier.EQ => BranchModifier.EQ
  }

  private def intOp(bytesOrOp: Either[Bytes, MemoryOperand]): Expression = bytesOrOp match {
    case Left(bytes) => const(bytes.asInt)
    case Right(op) => invokeVirtual(classOf[MemoryRef], "getInt", op.expression)
  }

  private def longOp(bytesOrOp: Either[Bytes, MemoryOperand]): Expression = bytesOrOp match {
    case Left(bytes) => const(bytes.asLong)
    case Right(op) => invokeVirtual(classOf[MemoryRef], "getLong", op.expression)
  }

  private implicit class MemoryOperandExtension(op: MemoryOperand) {
    def expression: Expression = op match {
      case MemoryOperand.Stack(offset) =>
        val frameOffset = invokeStatic(classOf[FrameOffset], "nonNegative", const(offset.get))
        invokeVirtual(classOf[StackMemory], "getRef", BytecodeCompiler.StackExpression, frameOffset)
      case MemoryOperand.Native(offset) =>
        ???
    }

    def putInt(value: Expression): Expression =
      invokeVirtual(classOf[MemoryRef], "putInt", expression, value)

    def putLong(value: Expression): Expression =
      invokeVirtual(classOf[MemoryRef], "putLong", expression, value)
  }
}
