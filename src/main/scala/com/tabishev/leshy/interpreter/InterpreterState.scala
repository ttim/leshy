package com.tabishev.leshy.interpreter

import java.nio.ByteBuffer

// don't use byte buffers, and all these asInt etc. Just treat everything as byte arrays for real
// create BytesRef type which is reference, and expose stackRef, heapRef, literalRef
class InterpreterState {
  private var stack: Array[Byte] = Array.fill(10)(0)
  private var mirror: ByteBuffer = ByteBuffer.wrap(stack)
  private var size: Int = 0
  private var offset: Int = 0

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

  def getStack(length: Int, address: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    System.arraycopy(stack, offset + address, bytes, 0, length)
    bytes
  }

  def putStack(index: Int, value: Array[Byte]): Unit = mirror.put(offset + index, value)
  def putStackInt(index: Int, value: Int): Unit = mirror.putInt(offset + index, value)
  def putStackLong(index: Int, value: Long): Unit = mirror.putLong(offset + index, value)

  def advance(offset: Int): Unit = this.offset += offset
  def retreat(offset: Int): Unit = this.offset -= offset

  def checkSize(size: Int): Unit = size == this.size - this.offset
}
