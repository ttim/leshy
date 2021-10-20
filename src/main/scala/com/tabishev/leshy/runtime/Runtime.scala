package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.*

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

final class Runtime {
  val stack = new StackMemory()
  val symbols = new Symbols()

  CommonSymbols.register(symbols)
}

class StackMemory {
  var memory: Memory = Memory.ofSize(10, ro = false)
  var size: Int = 0
  var offset: Int = 0

  def getCurrentStackFrame(): Array[Byte] = memory.get(offset, size - offset)

  def getRef(index: Int): MemoryRef =
    if (index >= 0) {
      assert(index < (size - offset))
      new MemoryRef(memory, offset + index)
    } else {
      assert((-index) < (size - offset))
      new MemoryRef(memory, size + index)
    }

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    if (size + extendSize > memory.size) memory = memory.extended(Math.min(memory.size, extendSize), ro = false)
    size += extendSize
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    assert(size - offset >= shrinkSize)
    memory.zero(size - shrinkSize, shrinkSize)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def clean(): Unit = {
    size = 0
    offset = 0
  }

  def append(bytes: Bytes): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.offset)

  def offset(newOffset: Int): Unit = {
    assert(offset >= 0)
    this.offset = newOffset
  }

  override def toString: String = s"[${memory.get(offset, size - offset).mkString(", ")}]"
}