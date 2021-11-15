package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.lang.reflect.Method
import scala.reflect.ClassTag

trait BytecodeExpression {
  def push(writer: MethodVisitor): BytecodeExpression.Kind
}

object BytecodeExpression {
  enum Kind {
    case Void
    case Object
    case Int
    case Long

    def retInst: Int = this match {
      case Void => Opcodes.RETURN
      case Object => Opcodes.ARETURN
      case Int => Opcodes.IRETURN
      case Long => Opcodes.LRETURN
    }

    def popInst: Option[Int] = this match {
      case Void => None
      case Object => Some(Opcodes.POP)
      case Int => Some(Opcodes.POP)
      case Long => Some(Opcodes.POP2)
    }

    def loadInst: Option[Int] = this match {
      case Void => None
      case Object => Some(Opcodes.ALOAD)
      case Int => Some(Opcodes.ILOAD)
      case Long => Some(Opcodes.LLOAD)
    }

    def sumInst: Option[Int] = this match {
      case Void => None
      case Object => None
      case Int => Some(Opcodes.IADD)
      case Long => Some(Opcodes.LADD)
    }

    def multInst: Option[Int] = this match {
      case Void => None
      case Object => None
      case Int => Some(Opcodes.IMUL)
      case Long => Some(Opcodes.LMUL)
    }

    def negateInst: Option[Int] = this match {
      case Void => None
      case Object => None
      case Int => Some(Opcodes.INEG)
      case Long => Some(Opcodes.LNEG)
    }
  }

  def kind[T: ClassTag]: Kind = kind(Type.getType(implicitly[ClassTag[T]].runtimeClass))

  def kind(tpe: Type): Kind = tpe.getSort match {
    case Type.VOID => Kind.Void
    case Type.OBJECT => Kind.Object
    case Type.INT => Kind.Int
    case Type.LONG => Kind.Long
  }
}

case class BytecodePrimitive private[bytecode] (kind: BytecodeExpression.Kind, value: Any) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    writer.visitLdcInsn(value)
    kind
  }
}

case class Field(isStatic: Boolean, name: String, owner: Type, tpe: Type) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    if (!isStatic) {
      writer.push(ThisInstance())
      writer.visitFieldInsn(Opcodes.GETFIELD, owner.getInternalName, name, tpe.getDescriptor)
    } else {
      ???
    }
    BytecodeExpression.kind(tpe)
  }
}

case class InvokeMethod(opcode: Int, clazz: Class[_], name: String, args: Seq[BytecodeExpression]) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    args.foreach { arg =>
      assert(arg.push(writer) != BytecodeExpression.Kind.Void)
    }
    writer.visitMethodInsn(opcode, Type.getType(clazz).getInternalName, name, Type.getMethodDescriptor(method()), false)
    BytecodeExpression.kind(Type.getType(method().getReturnType))
  }

  private def method(): Method = {
    val methods = clazz.getMethods.filter(_.getName == name)
    assert(methods.length == 1)
    methods(0)
  }
}

object InvokeMethod {
  def virtual(clazz: Class[_], name: String, args: BytecodeExpression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, args.toSeq)

  def static(clazz: Class[_], name: String, args: BytecodeExpression*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, args.toSeq)
}

case class Param(idx: Int, kind: BytecodeExpression.Kind) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    kind.loadInst.foreach { inst =>
      writer.visitVarInsn(inst, idx + 1)
    }
    kind
  }
}

object Param {
  def idx[T: ClassTag](idx: Int): Param = Param(idx, BytecodeExpression.kind[T])
}

case class ThisInstance() extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    BytecodeExpression.Kind.Object
  }
}

case class InvokeSuper(superClass: Class[_]) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(superClass), "<init>", "()V", false)
    BytecodeExpression.Kind.Void
  }
}

case class BytecodeBinaryOp(opcode: BytecodeExpression.Kind => Int, arg1: BytecodeExpression, arg2: BytecodeExpression) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    val kind = arg1.push(writer)
    val kind2 = arg2.push(writer)
    assert(kind == kind2)
    writer.visitInsn(opcode(kind))
    kind
  }
}

case class BytecodeUnaryOp(op: BytecodeExpression.Kind => Int, arg: BytecodeExpression) extends BytecodeExpression {
  override def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    val kind = arg.push(writer)
    writer.visitInsn(op(kind))
    kind
  }
}

object Ops {
  def negate(arg: BytecodeExpression): BytecodeExpression =
    BytecodeUnaryOp(_.negateInst.get, arg)
  def sum(arg1: BytecodeExpression, arg2: BytecodeExpression): BytecodeExpression =
    BytecodeBinaryOp(_.sumInst.get, arg1, arg2)
  def mult(arg1: BytecodeExpression, arg2: BytecodeExpression): BytecodeExpression =
    BytecodeBinaryOp(_.multInst.get, arg1, arg2)

  def int(value: Int): BytecodeExpression = BytecodePrimitive(BytecodeExpression.Kind.Int, value)
  def long(value: Long): BytecodeExpression =  BytecodePrimitive(BytecodeExpression.Kind.Long, value)
  def bool(value: Boolean): BytecodeExpression = BytecodePrimitive(BytecodeExpression.Kind.Int, if (value) 1 else 0)
}
