package com.tabishev.leshy.bytecode

import org.objectweb.asm.{Opcodes, Type}

import scala.reflect.ClassTag

enum BytecodeExpressionKind {
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

  def storeInst: Option[Int] = this match {
    case Void => None
    case Object => Some(Opcodes.ASTORE)
    case Int => Some(Opcodes.ISTORE)
    case Long => Some(Opcodes.LSTORE)
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

object BytecodeExpressionKind {
  def of[T: ClassTag]: BytecodeExpressionKind = of(Type.getType(implicitly[ClassTag[T]].runtimeClass))

  def of(tpe: Type): BytecodeExpressionKind = tpe.getSort match {
    case Type.VOID => BytecodeExpressionKind.Void
    case Type.OBJECT => BytecodeExpressionKind.Object
    case Type.INT => BytecodeExpressionKind.Int
    case Type.LONG => BytecodeExpressionKind.Long
  }
}
