package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.lang.common.{ConstInterpreter, Consts, Symbols}
import com.tabishev.leshy.runtime.FrameOffset

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
}

case class SpecializationContextConstInterpreter(sym: Symbols, ctx: SpecializationContext) extends ConstInterpreter {
  override def frameSize(): Int = ctx.stackSize
  override def symbols(): Symbols = sym
  override def isConst(from: FrameOffset, length: Int): Boolean = ctx.consts.isConst(from, length)
  override def get(from: FrameOffset, length: Int): Array[Byte] = ctx.consts.get(from, length)
}
