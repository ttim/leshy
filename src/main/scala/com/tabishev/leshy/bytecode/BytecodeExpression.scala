package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.lang.reflect.Method
import scala.reflect.ClassTag

trait BytecodeExpression[T] {
  def push(writer: MethodVisitor, value: T): BytecodeExpression.Kind
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
  }

  def kind[T: ClassTag]: Kind = kind(Type.getType(implicitly[ClassTag[T]].runtimeClass))

  def kind(tpe: Type): Kind = tpe.getSort match {
    case Type.VOID => Kind.Void
    case Type.OBJECT => Kind.Object
    case Type.INT => Kind.Int
    case Type.LONG => Kind.Long
  }

  def from[TIn, TOut: BytecodeExpression](fn: TIn => TOut): BytecodeExpression[TIn] = new BytecodeExpression[TIn] {
    override def push(writer: MethodVisitor, value: TIn): BytecodeExpression.Kind = writer.push(fn(value))
  }

  def from[TIn, TOut: BytecodeExpression](other: TOut): BytecodeExpression[TIn] = from(_ => other)
}

given intBytecodeExpression: BytecodeExpression[Int] with {
  override def push(writer: MethodVisitor, value: Int): BytecodeExpression.Kind = {
    writer.visitLdcInsn(value)
    BytecodeExpression.Kind.Int
  }
}

given longBytecodeExpression: BytecodeExpression[Long] with {
  override def push(writer: MethodVisitor, value: Long): BytecodeExpression.Kind = {
    writer.visitLdcInsn(value)
    BytecodeExpression.Kind.Long
  }
}

given booleanBytecodeExpression: BytecodeExpression[Boolean] with {
  override def push(writer: MethodVisitor, value: Boolean): BytecodeExpression.Kind = {
    writer.visitLdcInsn(if (value) 1 else 0)
    BytecodeExpression.Kind.Int
  }
}

case class Field(isStatic: Boolean, name: String, owner: Type, tpe: Type)

case class InvokeMethod(opcode: Int, clazz: Class[_], name: String, args: Seq[Value[_]])

object InvokeMethod {
  def virtualValues(clazz: Class[_], name: String, args: Value[_]*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, args.toSeq)

  def staticValues(clazz: Class[_], name: String, args: Value[_]*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, args.toSeq)

  def virtual[T1: BytecodeExpression](clazz: Class[_], name: String, arg1: T1): InvokeMethod =
    virtualValues(clazz, name, Value(arg1))

  def virtual[T1: BytecodeExpression, T2: BytecodeExpression](clazz: Class[_], name: String, arg1: T1, arg2: T2): InvokeMethod =
    virtualValues(clazz, name, Value(arg1), Value(arg2))

  def static[T1: BytecodeExpression](clazz: Class[_], name: String, arg1: T1): InvokeMethod =
    staticValues(clazz, name, Value(arg1))
}

case class Param(idx: Int, kind: BytecodeExpression.Kind)

object Param {
  def idx[T: ClassTag](idx: Int): Param = Param(idx, BytecodeExpression.kind[T])
}

case class ThisInstance()

case class InvokeSuper(superClass: Class[_])

given superBytecodeExpression: BytecodeExpression[InvokeSuper] with {
  override def push(writer: MethodVisitor, value: InvokeSuper): BytecodeExpression.Kind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(value.superClass), "<init>", "()V", false)
    BytecodeExpression.Kind.Void
  }
}

given thisBytecodeExpression: BytecodeExpression[ThisInstance] with {
  override def push(writer: MethodVisitor, value: ThisInstance): BytecodeExpression.Kind = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    BytecodeExpression.Kind.Object
  }
}

given fieldBytecodeExpression: BytecodeExpression[Field] with {
  override def push(writer: MethodVisitor, value: Field): BytecodeExpression.Kind = {
    if (!value.isStatic) {
      writer.push(ThisInstance())
      writer.visitFieldInsn(Opcodes.GETFIELD, value.owner.getInternalName, value.name, value.tpe.getDescriptor)
    } else {
      ???
    }
    BytecodeExpression.kind(value.tpe)
  }
}

given invokeMethodBytecodeExpression: BytecodeExpression[InvokeMethod] with {
  override def push(writer: MethodVisitor, value: InvokeMethod): BytecodeExpression.Kind = {
    value.args.foreach { arg =>
      assert(arg.push(writer) != BytecodeExpression.Kind.Void)
    }
    writer.visitMethodInsn(value.opcode, Type.getType(value.clazz).getInternalName, value.name, Type.getMethodDescriptor(method(value)), false)
    BytecodeExpression.kind(Type.getType(method(value).getReturnType))
  }

  private def method(value: InvokeMethod): Method = {
    val methods = value.clazz.getMethods.filter(_.getName == value.name)
    assert(methods.length == 1)
    methods(0)
  }
}

given paramBytecodeExpression: BytecodeExpression[Param] with {
  override def push(writer: MethodVisitor, value: Param): BytecodeExpression.Kind = {
    value.kind.loadInst.foreach { inst =>
      writer.visitVarInsn(inst, value.idx + 1)
    }
    value.kind
  }
}

case class Value[T: BytecodeExpression](value: T) {
  def push(writer: MethodVisitor): BytecodeExpression.Kind = implicitly[BytecodeExpression[T]].push(writer, value)
}

case class BytecodeSum[V1: BytecodeExpression, V2: BytecodeExpression](arg1: V1, arg2: V2) {
  def push(writer: MethodVisitor): BytecodeExpression.Kind = {
    val kind = implicitly[BytecodeExpression[V1]].push(writer, arg1)
    val kind2 = implicitly[BytecodeExpression[V2]].push(writer, arg2)
    assert(kind == kind2)
    writer.visitInsn(kind.sumInst.get)
    kind
  }
}

given sumBytecodeExpression[V1, V2]: BytecodeExpression[BytecodeSum[V1, V2]] with {
  override def push(writer: MethodVisitor, value: BytecodeSum[V1, V2]): BytecodeExpression.Kind = value.push(writer)
}
