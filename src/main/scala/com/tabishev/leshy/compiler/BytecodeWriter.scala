package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.intBytecodeExpression
import com.tabishev.leshy.bytecode.paramBytecodeExpression
import com.tabishev.leshy.bytecode.invokeMethodBytecodeExpression
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

given memoryOperandPushable: BytecodeExpression[MemoryOperand] = BytecodeExpression.from {
  case MemoryOperand.Stack(offset) =>
    InvokeMethod.virtual(classOf[StackMemory], "getRef", ContextStack(), offset)
  case MemoryOperand.Native(offset) =>
    ???
}

case class ContextStack()
case class ContextRuntime()

given runtimePushable: BytecodeExpression[ContextRuntime] =
  BytecodeExpression.from(Param.idx[Runtime](0))
given frameOffsetPushable: BytecodeExpression[FrameOffset] =
  BytecodeExpression.from(value => InvokeMethod.static(classOf[FrameOffset], "nonNegative", value.get))
given stackPushable: BytecodeExpression[ContextStack] =
  BytecodeExpression.from(InvokeMethod.virtual(classOf[Runtime], "stack", ContextRuntime()))
