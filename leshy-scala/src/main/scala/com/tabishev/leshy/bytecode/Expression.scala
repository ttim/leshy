package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.lang.reflect.Method
import scala.reflect.ClassTag

trait Expression {
  def push(writer: MethodVisitor): BytecodeExpressionKind
}

object Expression {
  def negate(arg: Expression): Expression =
    UnaryExpression(_.negateInst.get, arg)
  def sum(arg1: Expression, arg2: Expression): Expression =
    BinaryExpression(_.sumInst.get, arg1, arg2)
  def mult(arg1: Expression, arg2: Expression): Expression =
    BinaryExpression(_.multInst.get, arg1, arg2)

  def const(value: Int): Expression = ConstExpression(BytecodeExpressionKind.Int, value)
  def const(value: Long): Expression =  ConstExpression(BytecodeExpressionKind.Long, value)
  def const(value: Boolean): Expression = ConstExpression(BytecodeExpressionKind.Int, if (value) 1 else 0)

  def void(): Expression = VoidExpression

  def invokeVirtual(clazz: Class[_], name: String, args: Expression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, args.toSeq)

  def invokeStatic(clazz: Class[_], name: String, args: Expression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, args.toSeq)

  def local[T: ClassTag](idx: Int): Expression = Local(idx, BytecodeExpressionKind.of[T])
}

case class Cast(expression: Expression, clz: Class[_]) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = expression.push(writer)
    writer.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clz))
    kind
  }
}

case object VoidExpression extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = BytecodeExpressionKind.Void
}

case class ConstExpression private[bytecode](kind: BytecodeExpressionKind, value: Any) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitLdcInsn(value)
    kind
  }
}

case class Field(isStatic: Boolean, name: String, owner: Type, tpe: Type) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    if (!isStatic) {
      ThisInstance().push(writer)
      writer.visitFieldInsn(Opcodes.GETFIELD, owner.getInternalName, name, tpe.getDescriptor)
    } else {
      ???
    }
    BytecodeExpressionKind.of(tpe)
  }
}

case class InvokeMethod(opcode: Int, clazz: Class[_], name: String, args: Seq[Expression]) extends Expression {
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

case class Local(idx: Int, kind: BytecodeExpressionKind) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(kind.loadInst.get, idx)
    kind
  }
}

case class ThisInstance() extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    BytecodeExpressionKind.Object
  }
}

case class InvokeSuper(superClass: Class[_]) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(superClass), "<init>", "()V", false)
    BytecodeExpressionKind.Void
  }
}

case class BinaryExpression(opcode: BytecodeExpressionKind => Int, arg1: Expression, arg2: Expression) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = arg1.push(writer)
    val kind2 = arg2.push(writer)
    assert(kind == kind2)
    writer.visitInsn(opcode(kind))
    kind
  }
}

case class UnaryExpression(op: BytecodeExpressionKind => Int, arg: Expression) extends Expression {
  override def push(writer: MethodVisitor): BytecodeExpressionKind = {
    val kind = arg.push(writer)
    writer.visitInsn(op(kind))
    kind
  }
}
