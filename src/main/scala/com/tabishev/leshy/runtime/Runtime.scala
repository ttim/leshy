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
  private val initialSize = 10

  var memory: Memory = Memory.ofSize(initialSize, ro = false)
  var marks: Memory = Memory.ofSize(initialSize, ro = false)

  var size: Int = 0
  var frameOffset: Int = 0

  def stackFrameSize(): Int = size - frameOffset
  def getCurrentStackFrame(): Array[Byte] = memory.get(frameOffset, size - frameOffset)

  private def calcAbsoluteOffset(offset: Int): Int =
    if (offset >= 0) {
      assert(offset < (size - frameOffset))
      frameOffset + offset
    } else {
      assert((-offset) <= (size - frameOffset))
      size + offset
    }

  def getRef(offset: Int): MemoryRef = new MemoryRef(memory, calcAbsoluteOffset(offset))

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    if (size + extendSize > memory.size) {
      val memoryExtendSize = Math.min(memory.size, extendSize)
      memory = memory.extended(memoryExtendSize, ro = false)
      marks = marks.extended(memoryExtendSize, ro = false)
    }
    size += extendSize
    markConst(-extendSize, extendSize, isConst = true)
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    assert(size - frameOffset >= shrinkSize)
    memory.fill(size - shrinkSize, shrinkSize, 0)
    markConst(-shrinkSize, shrinkSize, isConst = false)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def clean(): Unit = {
    size = 0
    frameOffset = 0
  }

  def append(bytes: Bytes, isConst: Boolean): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
    markConst(size - bytes.length(), bytes.length(), isConst)
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.frameOffset)

  def offset(newOffset: Int): Unit = {
    assert(frameOffset >= 0)
    this.frameOffset = newOffset
  }

  override def toString: String = s"[${memory.get(frameOffset, size - frameOffset).mkString(", ")}]"

  // const actions
  def markConst(offset: Int, length: Int, isConst: Boolean): Unit = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.fill(absoluteOffset, length, if (isConst) 1 else 0)
  }

  def isConst(offset: Int, length: Int): Boolean = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.allEquals(absoluteOffset, length, 1)
  }

  def stackFrameConsts(): Map[Int, Byte] =
    marks.nonEqualOffsets(frameOffset, size - frameOffset, 1).map { offset =>
      (offset - frameOffset, memory.getByte(frameOffset))
    }.toMap
}