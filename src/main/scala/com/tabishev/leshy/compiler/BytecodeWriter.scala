package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{FrameOffset, MemoryRef, Runtime, StackMemory}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import scala.reflect.ClassTag

extension (writer: MethodVisitor) {
  def pushMemoryRef(operand: MemoryOperand): Unit = operand match {
    case MemoryOperand.Stack(offset) =>
      pushStack()
      pushFrameOffset(offset)
      invoke[StackMemory]("getRef")
    case MemoryOperand.Native(offset) =>
      ???
  }

  def pushStack(): Unit = {
    writer.visitVarInsn(Opcodes.ALOAD, 1) // runtime
    invoke[Runtime]("stack")
  }

  def pushFrameOffset(offset: FrameOffset): Unit = {
    writer.visitLdcInsn(offset.get)
    writer.visitMethodInsn(Opcodes.INVOKESTATIC, Type.getType(classOf[Interop]).getInternalName, "frameOffset", Type.getMethodDescriptor(Type.getType(classOf[FrameOffset]), Type.INT_TYPE))
  }

  def invoke[T: ClassTag](name: String): Unit = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    writer.visitMethodInsn(Opcodes.INVOKEVIRTUAL, Type.getType(clazz).getInternalName, name, getMethodDescriptor(clazz, name), false)
  }

  private def getMethodDescriptor(clazz: Class[_], name: String): String = {
    val methods = clazz.getMethods.filter(_.getName == name)
    assert(methods.length == 1)
    Type.getMethodDescriptor(methods(0))
  }
}
