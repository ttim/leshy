package com.tabishev.leshy.runtime

final class StackMemory {
  private val initialSize = 1000

  private[runtime] var memory: Memory = Memory.ofSize(initialSize, ro = false)

  private var size: Int = 0
  private var frameOffset: Int = 0

  def frameSize(): Int = size - frameOffset
  def currentStackFrame(): Array[Byte] = memory.get(frameOffset, size - frameOffset)

  def isEmpty(): Boolean = size == 0

  def getFrameOffset(): Int = frameOffset

  def getRef(offset: FrameOffset): MemoryRef = new MemoryRef(memory, frameOffset + offset.get)

  def setFramesize(newFrameSize: Int): Unit = {
    assert(newFrameSize >= 0)

    val newSize = frameOffset + newFrameSize
    if (newSize > size) {
      // extend if needed
      if (newSize > memory.size) {
        val memoryExtendSize = Math.min(memory.size, newSize - memory.size)
        memory = memory.extended(memoryExtendSize, ro = false)
      }
      size = newSize
    } else if (newSize < size) {
      // shrink
      size = newSize
    }
  }

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    setFramesize(frameSize() + extendSize)
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    setFramesize(frameSize() - shrinkSize)
  }

  def clean(): Unit = {
    size = 0
    frameOffset = 0
  }

  def append(bytes: Bytes): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
  }

  def moveFrame(offset: Int): Unit = frameOffset += offset
}