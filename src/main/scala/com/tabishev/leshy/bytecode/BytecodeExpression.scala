package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.lang.reflect.Method
import scala.reflect.ClassTag

trait BytecodeExpression {
  def push(writer: MethodVisitor): BytecodeExpressionKind
}

object BytecodeExpression {
  def negate(arg: BytecodeExpression): BytecodeExpression =
    BytecodeUnaryOp(_.negateInst.get, arg)
  def sum(arg1: BytecodeExpression, arg2: BytecodeExpression): BytecodeExpression =
    BytecodeBinaryOp(_.sumInst.get, arg1, arg2)
  def mult(arg1: BytecodeExpression, arg2: BytecodeExpression): BytecodeExpression =
    BytecodeBinaryOp(_.multInst.get, arg1, arg2)

  def const(value: Int): BytecodeExpression = BytecodeConst(BytecodeExpressionKind.Int, value)
  def const(value: Long): BytecodeExpression =  BytecodeConst(BytecodeExpressionKind.Long, value)
  def const(value: Boolean): BytecodeExpression = BytecodeConst(BytecodeExpressionKind.Int, if (value) 1 else 0)

  def invokeVirtual(clazz: Class[_], name: String, args: BytecodeExpression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, args.toSeq)

  def invokeStatic(clazz: Class[_], name: String, args: BytecodeExpression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, args.toSeq)

  def local[T: ClassTag](idx: Int): BytecodeExpression = Local(idx, BytecodeExpressionKind.of[T])
}

case class Cast(expression: BytecodeExpression, clz: Class[_]) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = writer.push(expression)
    writer.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clz))
    kind
  }
}

case class BytecodeConst private[bytecode](kind: BytecodeExpressionKind, value: Any) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitLdcInsn(value)
    kind
  }
}

case class Field(isStatic: Boolean, name: String, owner: Type, tpe: Type) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    if (!isStatic) {
      writer.push(ThisInstance())
      writer.visitFieldInsn(Opcodes.GETFIELD, owner.getInternalName, name, tpe.getDescriptor)
    } else {
      ???
    }
    BytecodeExpressionKind.of(tpe)
  }
}

case class InvokeMethod(opcode: Int, clazz: Class[_], name: String, args: Seq[BytecodeExpression]) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    args.foreach { arg =>
      assert(arg.push(writer) != BytecodeExpressionKind.Void)
    }
    writer.visitMethodInsn(opcode, Type.getType(clazz).getInternalName, name, Type.getMethodDescriptor(method()), false)
    BytecodeExpressionKind.of(Type.getType(method().getReturnType))
  }

  private def method(): Method = {
    val methods = clazz.getMethods.filter(_.getName == name)
    assert(methods.length == 1)
    methods(0)
  }
}

case class Local(idx: Int, kind: BytecodeExpressionKind) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(kind.loadInst.get, idx)
    kind
  }
}

case class ThisInstance() extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    BytecodeExpressionKind.Object
  }
}

case class InvokeSuper(superClass: Class[_]) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(superClass), "<init>", "()V", false)
    BytecodeExpressionKind.Void
  }
}

case class BytecodeBinaryOp(opcode: BytecodeExpressionKind => Int, arg1: BytecodeExpression, arg2: BytecodeExpression) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = arg1.push(writer)
    val kind2 = arg2.push(writer)
    assert(kind == kind2)
    writer.visitInsn(opcode(kind))
    kind
  }
}

case class BytecodeUnaryOp(op: BytecodeExpressionKind => Int, arg: BytecodeExpression) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = arg.push(writer)
    writer.visitInsn(op(kind))
    kind
  }
}
