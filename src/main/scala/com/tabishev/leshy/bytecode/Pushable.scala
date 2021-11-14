package com.tabishev.leshy.bytecode

import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

import java.lang.reflect.Method
import scala.reflect.ClassTag

trait Pushable[T] {
  def push(writer: MethodVisitor, value: T): Unit

  def kind(value: T): Pushable.Kind
}

object Pushable {
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
  }

  def kind[T: ClassTag]: Kind = kind(Type.getType(implicitly[ClassTag[T]].runtimeClass))

  def kind(tpe: Type): Kind = tpe.getSort match {
    case Type.VOID => Kind.Void
    case Type.OBJECT => Kind.Object
    case Type.INT => Kind.Int
    case Type.LONG => Kind.Long
  }

  def from[TIn, TOut: Pushable](fn: TIn => TOut): Pushable[TIn] = new Pushable[TIn] {
    override def push(writer: MethodVisitor, value: TIn): Unit = writer.push(fn(value))
    override def kind(value: TIn): Kind = implicitly[Pushable[TOut]].kind(fn(value))
  }

  def from[TIn, TOut: Pushable](other: TOut): Pushable[TIn] = from(_ => other)
}

given intPushable: Pushable[Int] with {
  override def push(writer: MethodVisitor, value: Int): Unit = writer.visitLdcInsn(value)
  override def kind(value: Int): Pushable.Kind = Pushable.Kind.Int
}

given longPushable: Pushable[Long] with {
  override def push(writer: MethodVisitor, value: Long): Unit = writer.visitLdcInsn(value)
  override def kind(value: Long): Pushable.Kind = Pushable.Kind.Long
}

case class Field(isStatic: Boolean, name: String, owner: Type, tpe: Type)

case class InvokeMethod(opcode: Int, clazz: Class[_], name: String, args: Seq[Value[_]])

object InvokeMethod {
  def virtualValues(clazz: Class[_], name: String, args: Value[_]*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, args.toSeq)

  def staticValues(clazz: Class[_], name: String, args: Value[_]*): InvokeMethod =
    InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, args.toSeq)

  def virtual[T1: Pushable](clazz: Class[_], name: String, arg1: T1): InvokeMethod =
    virtualValues(clazz, name, Value(arg1))

  def virtual[T1: Pushable, T2: Pushable](clazz: Class[_], name: String, arg1: T1, arg2: T2): InvokeMethod =
    virtualValues(clazz, name, Value(arg1), Value(arg2))

  def static[T1: Pushable](clazz: Class[_], name: String, arg1: T1): InvokeMethod =
    staticValues(clazz, name, Value(arg1))
}

case class Param(idx: Int, kind: Pushable.Kind)

object Param {
  def idx[T: ClassTag](idx: Int): Param = Param(idx, Pushable.kind[T])
}

case class ThisInstance()

case class InvokeSuper(superClass: Class[_])

given superPushable: Pushable[InvokeSuper] with {
  override def push(writer: MethodVisitor, value: InvokeSuper): Unit = {
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(value.superClass), "<init>", "()V", false)
  }

  override def kind(value: InvokeSuper): Pushable.Kind = Pushable.Kind.Void
}

given thisPushable: Pushable[ThisInstance] with {
  override def push(writer: MethodVisitor, value: ThisInstance): Unit = writer.visitVarInsn(Opcodes.ALOAD, 0)
  override def kind(value: ThisInstance): Pushable.Kind = Pushable.Kind.Object
}

given fieldPushable: Pushable[Field] with {
  override def push(writer: MethodVisitor, value: Field): Unit = {
    if (!value.isStatic) {
      writer.push(ThisInstance())
      writer.visitFieldInsn(Opcodes.GETFIELD, value.owner.getInternalName, value.name, value.tpe.getDescriptor)
    } else {
      ???
    }
  }
  override def kind(value: Field): Pushable.Kind = Pushable.kind(value.tpe)
}

given invokeMethodPushable: Pushable[InvokeMethod] with {
  override def push(writer: MethodVisitor, value: InvokeMethod): Unit = {
    value.args.foreach(_.push(writer))
    assert(value.args.forall(_.kind != Pushable.Kind.Void))
    writer.visitMethodInsn(value.opcode, Type.getType(value.clazz).getInternalName, value.name, Type.getMethodDescriptor(method(value)), false)
  }

  override def kind(value: InvokeMethod): Pushable.Kind = Pushable.kind(Type.getType(method(value).getReturnType))

  private def method(value: InvokeMethod): Method = {
    val methods = value.clazz.getMethods.filter(_.getName == value.name)
    assert(methods.length == 1)
    methods(0)
  }
}

given paramPushable: Pushable[Param] with {
  override def push(writer: MethodVisitor, value: Param): Unit =
    value.kind.loadInst.foreach { inst =>
      writer.visitVarInsn(inst, value.idx + 1)
    }

  override def kind(value: Param): Pushable.Kind = value.kind
}

case class Value[T: Pushable](value: T) {
  def push(writer: MethodVisitor): Unit = implicitly[Pushable[T]].push(writer, value)
  def kind: Pushable.Kind = implicitly[Pushable[T]].kind(value)
}
