package com.tabishev.leshy.compiler

import com.tabishev.leshy.runtime.{Consts, FrameOffset, Runtime}

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

  def restore(runtime: Runtime): Unit = {
    val (size, consts) = get()
    assert(runtime.stack.frameSize() == size)
    MemoryOperand.Stack(FrameOffset.Zero).markConst(runtime, size, isConst = false)
    consts.asMap().foreach { case (offsetRaw, value) =>
      val offset = FrameOffset.nonNegative(offsetRaw)
      assert(runtime.stack.getRef(offset).getByte() == value)
      MemoryOperand.Stack(offset).markConst(runtime, 1, isConst = true)
    }
  }
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

  def current(runtime: Runtime): SpecializationContext =
    SpecializationContext.from(runtime.stack.frameSize(), runtime.consts.get())

  def fnCall(caller: SpecializationContext, offset: Int, callee: SpecializationContext): SpecializationContext = {
    val (_, callerConsts) = caller.get()
    val (calleeSize, calleeConsts) = callee.get()
    SpecializationContext.from(offset + calleeSize, Consts.fnCall(callerConsts, offset, calleeConsts))
  }

  def offset(caller: SpecializationContext, offset: Int): SpecializationContext = {
    val (size, consts) = caller.get()
    val calleeConsts = consts.asMap().collect {
      case (callerOffset, value) if callerOffset >= offset => (callerOffset - offset, value)
    }
    SpecializationContext.from(size - offset, Consts.fromMap(calleeConsts))
  }
}
