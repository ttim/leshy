package com.tabishev.leshy.compiler

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.intPushable
import com.tabishev.leshy.bytecode.paramPushable
import com.tabishev.leshy.bytecode.invokeMethodPushable
import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

given memoryOperandPushable: Pushable[MemoryOperand] = Pushable.from {
  case MemoryOperand.Stack(offset) =>
    InvokeMethod.virtual(classOf[StackMemory], "getRef", ContextStack(), offset)
  case MemoryOperand.Native(offset) =>
    ???
}

case class ContextStack()
case class ContextRuntime()

given runtimePushable: Pushable[ContextRuntime] =
  Pushable.from(Param.idx[Runtime](0))
given frameOffsetPushable: Pushable[FrameOffset] =
  Pushable.from(value => InvokeMethod.static(classOf[FrameOffset], "nonNegative", value.get))
given stackPushable: Pushable[ContextStack] =
  Pushable.from(InvokeMethod.virtual(classOf[Runtime], "stack", ContextRuntime()))
