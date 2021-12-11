package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.lang.common.{ConstInterpreter, Consts, Symbols}
import com.tabishev.leshy.node.{Command, MemoryOperand, Unify}
import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

import scala.collection.mutable

case class SpecializationContext(stackSize: Int, consts: Consts) {
  override def toString: String = s"spec[$stackSize, $consts)]"

  def fnCall(offset: FrameOffset, callee: SpecializationContext): SpecializationContext =
    SpecializationContext(offset.get + callee.stackSize, consts.returnFromCall(offset, callee.consts))

  def offset(offset: FrameOffset): SpecializationContext =
    SpecializationContext(stackSize - offset.get, consts.call(offset))

  def setSize(newSize: Int): SpecializationContext =
    SpecializationContext(newSize,
      if (newSize > stackSize)
        consts.markConsts(FrameOffset.nonNegative(stackSize), Array.fill[Byte](newSize - stackSize)(0))
      else
        consts.unmarkConsts(FrameOffset.nonNegative(newSize), stackSize - newSize)
    )

  def afterCommand(command: Command): SpecializationContext =
    SpecializationContext(stackSize, {
      val output = command.output.get
      Unify.command(command) match {
        case Some(Command.Set(length, dst, op: Bytes)) =>
          markConst(consts, output.dst, op.get())
        case Some(_) =>
          throw new IllegalStateException("unify suppose to return Command.Set with bytes")
        case None =>
          unmarkConst(consts, output.dst, output.length)
      }
    })

  def notSpecialize(dst: MemoryOperand, length: Int): SpecializationContext =
    SpecializationContext(stackSize, unmarkConst(consts, dst, length))

  private[compiler] def markConst(consts: Consts, op: MemoryOperand, bytes: Array[Byte]): Consts = op match {
    case MemoryOperand.Stack(offset) =>
      consts.markConsts(offset, bytes)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }

  private[compiler] def unmarkConst(consts: Consts, op: MemoryOperand, length: Int): Consts = op match {
    case MemoryOperand.Stack(offset) =>
      consts.unmarkConsts(offset, length)
    case MemoryOperand.Native(offset) =>
      // do nothing
      consts
  }
}

case class SpecializationContextConstInterpreter(sym: Symbols, ctx: SpecializationContext) extends ConstInterpreter {
  override def frameSize(): Int = ctx.stackSize
  override def symbols(): Symbols = sym
  override def isConst(from: FrameOffset, length: Int): Boolean = ctx.consts.isConst(from, length)
  override def get(from: FrameOffset, length: Int): Array[Byte] = ctx.consts.get(from, length)
}
