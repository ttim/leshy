package com.tabishev.leshy.compiler

import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.runtime.{Consts, FrameOffset, Runtime, Symbols}

import scala.collection.mutable

case class SpecializationContext private (id: Int) {
  override def toString: String = {
    val (stackSize, consts) = get()
    s"spec[$stackSize, $consts)]"
  }

  override def hashCode(): Int = id
  override def equals(obj: Any): Boolean = obj.isInstanceOf[SpecializationContext] &&
    obj.asInstanceOf[SpecializationContext].id == id

  def get(): (Int, Consts) = SpecializationContext.idToContext(id)
}

object SpecializationContext {
  private var maxId = 1
  private val contextToId = mutable.HashMap[(Int, Consts), Int]()
  private val idToContext = mutable.HashMap[Int, (Int, Consts)]()

  def from(stackSize: Int, consts: Consts): SpecializationContext = {
    val key = (stackSize, consts)
    if (contextToId.contains(key)) SpecializationContext(contextToId(key)) else {
      val newId = maxId
      maxId += 1
      contextToId.put(key, newId)
      idToContext.put(newId, key)
      SpecializationContext(newId)
    }
  }

  def fnCall(caller: SpecializationContext, offset: FrameOffset, callee: SpecializationContext): SpecializationContext = {
    val (_, callerConsts) = caller.get()
    val (calleeSize, calleeConsts) = callee.get()
    SpecializationContext.from(offset.get + calleeSize, callerConsts.returnFromCall(offset, calleeConsts))
  }

  def offset(caller: SpecializationContext, offset: FrameOffset): SpecializationContext = {
    val (size, consts) = caller.get()
    SpecializationContext.from(size - offset.get, consts.call(offset))
  }
}

case class SpecializationContextConstInterpreter(sym: Symbols, ctx: SpecializationContext) extends ConstInterpreter {
  private val (size, consts) = ctx.get()

  override def frameSize(): Int = size
  override def symbols(): Symbols = sym
  override def isConst(from: FrameOffset, length: Int): Boolean = consts.isConst(from, length)
  override def get(from: FrameOffset, length: Int): Array[Byte] = consts.get(from, length)
}
