package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.runtime.{Consts, FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes, Type}
import com.tabishev.leshy.bytecode.*
import com.tabishev.leshy.bytecode.BytecodeExpression.*

abstract class Execution {
  def execute(runtime: Runtime): Unit
  def write(writer: MethodVisitor): Unit

  def specialize(before: SpecializationContext): SpecializationContext
}

abstract class BranchExecution {
  def execute(runtime: Runtime): Boolean
  def generate(writer: MethodVisitor, ifTrue: Label): Unit
}

trait UpdatesOnlyConsts extends Execution {
  final override def specialize(before: SpecializationContext): SpecializationContext =
    SpecializationContext(before.stackSize, updateConsts(before.consts))

  def updateConsts(consts: Consts): Consts
}

abstract class BinaryIntExecution extends Execution with UpdatesOnlyConsts {
  val dst: MemoryOperand
  val op1: IntProvider
  val op2: IntProvider

  def eval(arg1: Int, arg2: Int): Int
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(op1.get(runtime), op2.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(dst.putInt(expression))

  override final def updateConsts(consts: Consts): Consts = (op1, op2) match {
    case (IntProvider.Const(v1), IntProvider.Const(v2)) =>
      dst.markConst(consts, Bytes.fromInt(eval(v1, v2)).get())
    case _ =>
      dst.unmarkConst(consts, 4)
  }
}

abstract class BinaryLongExecution extends Execution with UpdatesOnlyConsts {
  val dst: MemoryOperand
  val op1: LongProvider
  val op2: LongProvider

  def eval(arg1: Long, arg2: Long): Long
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(op1.get(runtime), op2.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(dst.putLong(expression))

  override final def updateConsts(consts: Consts): Consts = (op1, op2) match {
    case (LongProvider.Const(v1), LongProvider.Const(v2)) =>
      dst.markConst(consts, Bytes.fromLong(eval(v1, v2)).get())
    case _ =>
      dst.unmarkConst(consts, 8)
  }
}

abstract class UnaryIntExecution extends Execution with UpdatesOnlyConsts {
  val dst: MemoryOperand
  val src: IntProvider

  def eval(arg: Int): Int
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putInt(eval(src.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(dst.putInt(expression))

  override final def updateConsts(consts: Consts): Consts = src match {
    case IntProvider.Const(v) =>
      dst.markConst(consts, Bytes.fromInt(eval(v)).get())
    case _ =>
      dst.unmarkConst(consts, 4)
  }
}

abstract class UnaryLongExecution extends Execution with UpdatesOnlyConsts {
  val dst: MemoryOperand
  val src: LongProvider

  def eval(arg: Long): Long
  val expression: BytecodeExpression

  override final def execute(runtime: Runtime): Unit =
    dst.materialize(runtime).putLong(eval(src.get(runtime)))
  override final def write(writer: MethodVisitor): Unit =
    writer.statement(dst.putLong(expression))

  override final def updateConsts(consts: Consts): Consts = src match {
    case LongProvider.Const(v) =>
      dst.markConst(consts, Bytes.fromLong(eval(v)).get())
    case _ =>
      dst.unmarkConst(consts, 8)
  }
}
