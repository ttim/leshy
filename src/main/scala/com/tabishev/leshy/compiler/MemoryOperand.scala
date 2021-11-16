package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode.BytecodeExpression
import com.tabishev.leshy.runtime.{Consts, FrameOffset, MemoryRef, Runtime}
import org.objectweb.asm.{MethodVisitor, Opcodes}

enum MemoryOperand {
  case Stack(offset: FrameOffset)
  case Native(stackOffset: FrameOffset)

  def materialize(runtime: Runtime): MemoryRef = this match {
    case MemoryOperand.Stack(offset) => runtime.stack.getRef(offset)
    case MemoryOperand.Native(offset) => ???
  }

  def markConst(consts: Consts, bytes: Array[Byte]): Consts = this match {
    case MemoryOperand.Stack(offset) =>
      consts.markConsts(offset, bytes)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }

  def unmarkConst(consts: Consts, length: Int): Consts = this match {
    case MemoryOperand.Stack(offset) =>
      consts.unmarkConsts(offset, length)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }
}

sealed abstract class IntProvider {
  def get(runtime: Runtime): Int
  def expression: BytecodeExpression
}

object IntProvider {
  final case class Const(value: Int) extends IntProvider {
    override def get(runtime: Runtime): Int = value
    override def expression: BytecodeExpression = BytecodeExpression.const(value)
  }

  final case class Operand(value: MemoryOperand) extends IntProvider {
    override def get(runtime: Runtime): Int = value.materialize(runtime).getInt()
    override def expression: BytecodeExpression = MemoryOps.getInt(value)
  }

  def create(constOrOperand: Int | MemoryOperand): IntProvider = constOrOperand match {
    case const: Int => new Const(const)
    case operand: MemoryOperand => new Operand(operand)
  }
}

sealed abstract class LongProvider {
  def get(runtime: Runtime): Long
  def expression: BytecodeExpression
}

object LongProvider {
  final case class Const(value: Long) extends LongProvider {
    override def get(runtime: Runtime): Long = value
    override def expression: BytecodeExpression = BytecodeExpression.const(value)
  }

  final case class Operand(value: MemoryOperand) extends LongProvider {
    override def get(runtime: Runtime): Long = value.materialize(runtime).getLong()
    override def expression: BytecodeExpression = MemoryOps.getLong(value)
  }

  def create(constOrOperand: Long | MemoryOperand): LongProvider = constOrOperand match {
    case const: Long => new Const(const)
    case operand: MemoryOperand => new Operand(operand)
  }
}

// todo: write about premature optimization: there've been 0 reason to optimize either for interpreter or compiler nodes...