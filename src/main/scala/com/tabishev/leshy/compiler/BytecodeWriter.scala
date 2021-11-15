package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.BytecodeExpression._
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

object MemoryOps {
  val Runtime: BytecodeExpression = param[Runtime](0)
  val Stack: BytecodeExpression = invokeVirtual(classOf[Runtime], "stack", Runtime)

  def frameOffset(offset: FrameOffset): BytecodeExpression =
    invokeStatic(classOf[FrameOffset], "nonNegative", const(offset.get))

  def memoryOperand(operand: MemoryOperand): BytecodeExpression = operand match {
    case MemoryOperand.Stack(offset) =>
      invokeVirtual(classOf[StackMemory], "getRef", Stack, frameOffset(offset))
    case MemoryOperand.Native(offset) =>
      ???
  }

  def getInt(op: MemoryOperand): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "getInt", memoryOperand(op))
  def putInt(op: MemoryOperand, value: BytecodeExpression): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "putInt", memoryOperand(op), value)
  def getLong(op: MemoryOperand): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "getLong", memoryOperand(op))
  def putLong(op: MemoryOperand, value: BytecodeExpression): BytecodeExpression =
    invokeVirtual(classOf[MemoryRef], "putLong", memoryOperand(op), value)
}
