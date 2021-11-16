package com.tabishev.leshy.compiler

import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.runtime.{Consts, FrameOffset, Runtime, Symbols}

import scala.collection.mutable

case class SpecializationContext(stackSize: Int, consts: Consts) {
  override def toString: String = s"spec[$stackSize, $consts)]"

  def fnCall(offset: FrameOffset, callee: SpecializationContext): SpecializationContext =
    SpecializationContext(offset.get + callee.stackSize, consts.returnFromCall(offset, callee.consts))

  def offset(offset: FrameOffset): SpecializationContext =
    SpecializationContext(stackSize - offset.get, consts.call(offset))
}

case class SpecializationContextConstInterpreter(sym: Symbols, ctx: SpecializationContext) extends ConstInterpreter {
  override def frameSize(): Int = ctx.stackSize
  override def symbols(): Symbols = sym
  override def isConst(from: FrameOffset, length: Int): Boolean = ctx.consts.isConst(from, length)
  override def get(from: FrameOffset, length: Int): Array[Byte] = ctx.consts.get(from, length)
}
