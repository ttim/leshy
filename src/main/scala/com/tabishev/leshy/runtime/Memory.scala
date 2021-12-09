package com.tabishev.leshy.runtime

import java.nio.{ByteBuffer, ByteOrder}

// todo: use MemoryAddress, MemorySegment & MemoryAccess instead?
final class Memory private (val capacity: Int, val ro: Boolean) {
  private var unloaded: Boolean = false
  private val bb = ByteBuffer.allocate(capacity).order(ByteOrder.LITTLE_ENDIAN)

  def size: Int = read {
    bb.limit()
  }

  def putInt(offset: Int, value: Int): Unit = write {
    bb.putInt(offset, value)
  }
  def putLong(offset: Int, value: Long): Unit = write {
    bb.putLong(offset, value)
  }
  def putBytes(offset: Int, value: Bytes): Unit = write {
    value.copyTo(bb, offset)
  }
  def put(offset: Int, value: Array[Byte]): Unit = write {
    bb.put(offset, value, 0, value.length)
  }

  def getByte(offset: Int): Byte = read {
    bb.get(offset)
  }
  def getInt(offset: Int): Int = read {
    bb.getInt(offset)
  }
  def getLong(offset: Int): Long = read {
    bb.getLong(offset)
  }
  def get(offset: Int, length: Int): Array[Byte] = read {
    val copyBytes = Array.fill[Byte](length)(0)
    bb.get(offset, copyBytes, 0, length)
    copyBytes
  }

  def extended(extendSize: Int, ro: Boolean): Memory = {
    read {
      val newMemory = new Memory(capacity + extendSize, ro)
      newMemory.bb.put(0, bb, 0, capacity)
      unloaded = true
      newMemory
    }
  }

  private inline def write[T](inline op: T): T = {
    assert(!unloaded && !ro)
    op
  }
  private inline def read[T](inline op: T): T = {
    assert(!unloaded)
    op
  }
}

object Memory {
  def ofSize(size: Int, ro: Boolean): Memory = new Memory(size, ro)
  def ofBytes(bytes: Array[Byte], ro: Boolean): Memory = {
    val mem = new Memory(bytes.length, ro)
    mem.bb.put(bytes)
    mem
  }
}

final class MemoryRef(val memory: Memory, val offset: Int) {
  def putInt(value: Int): Unit = memory.putInt(offset ,value)
  def putLong(value: Long): Unit = memory.putLong(offset, value)
  def put(value: Array[Byte]): Unit = memory.put(offset, value)

  def getInt(): Int = memory.getInt(offset)
  def getLong(): Long = memory.getLong(offset)
  def getByte(): Byte = memory.getByte(offset)
  def get(length: Int): Array[Byte] = memory.get(offset, length)
}