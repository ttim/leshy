package com.tabishev.leshy.interpreter

import java.nio.ByteBuffer

class MemoryRef(buffer: ByteBuffer, index: Int) {
  def putInt(value: Int): Unit = buffer.putInt(index, value)
  def putLong(value: Long): Unit = buffer.putLong(index, value)
  def put(value: Array[Byte]): Unit = buffer.put(index, value)
}

// don't use byte buffers, and all these asInt etc. Just treat everything as byte arrays for real
// create BytesRef type which is reference, and expose stackRef, heapRef, literalRef
class InterpreterState {
  val stack = new StackMemory()

  def getStack(length: Int, address: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    System.arraycopy(stack.stack, stack.offset + address, bytes, 0, length)
    bytes
  }
}

class StackMemory {
  var stack: Array[Byte] = Array.fill(10)(0)
  var mirror: ByteBuffer = ByteBuffer.wrap(stack)
  private var size: Int = 0
  var offset: Int = 0

  def getRef(index: Int): MemoryRef = new MemoryRef(mirror, offset + index)

  def extend(extendSize: Int): Unit = {
    if (size + extendSize <= stack.length) size += extendSize else {
      val newStack = Array.fill[Byte](stack.length * 2)(0)
      System.arraycopy(stack, 0, newStack, 0, stack.length)
      stack = newStack
      mirror = ByteBuffer.wrap(stack)
      extend(extendSize)
    }
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(size - offset >= shrinkSize)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def append(bytes: Array[Byte]): Unit = {
    extend(bytes.length)
    System.arraycopy(bytes, 0, stack, size - bytes.length, bytes.length)
  }

  def checkSize(size: Int): Unit = size == this.size - this.offset

  def advance(offset: Int): Unit = this.offset += offset
  def retreat(offset: Int): Unit = this.offset -= offset
}