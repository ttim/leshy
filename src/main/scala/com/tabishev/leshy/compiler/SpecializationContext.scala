package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{Consts, Runtime}

import scala.collection.mutable

case class SpecializationContext private (id: Int) {
  override def toString: String = {
    val (stackSize, consts) = SpecializationContext.get(this)
    s"spec[$stackSize, $consts)]"
  }

  override def hashCode(): Int = id
  override def equals(obj: Any): Boolean = obj.isInstanceOf[SpecializationContext] &&
    obj.asInstanceOf[SpecializationContext].id == id
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

  def get(ctx: SpecializationContext): (Int, Consts) = idToContext(ctx.id)

  def current(runtime: Runtime): SpecializationContext =
    SpecializationContext.from(runtime.stack.stackFrameSize(), runtime.stack.stackFrameConsts())

  def fnCall(caller: SpecializationContext, offset: Int, callee: SpecializationContext): SpecializationContext = {
    val (_, callerConsts) = get(caller)
    val (calleeSize, calleeConsts) = get(callee)
    SpecializationContext.from(offset + calleeSize, Consts.fnCall(callerConsts, offset, calleeConsts))
  }

  def offset(caller: SpecializationContext, offset: Int): SpecializationContext = {
    val (size, consts) = SpecializationContext.get(caller)
    val calleeConsts = consts.asMap().collect {
      case (callerOffset, value) if callerOffset >= offset => (callerOffset - offset, value)
    }
    SpecializationContext.from(size - offset, Consts.fromMap(calleeConsts))
  }
}
