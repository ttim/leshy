package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}

// todo: use MemoryAddress, MemorySegment & MemoryAccess instead?
final class Memory private (val bytes: Array[Byte], val ro: Boolean) {
  private var unloaded: Boolean = false
  private val mirror = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  def size: Int = read {
    bytes.length
  }

  def fill(offset: Int, length: Int, value: Byte): Unit = write {
    java.util.Arrays.fill(bytes, offset, offset + length, value)
  }

  def allEquals(offset: Int, length: Int, value: Byte): Boolean = read {
    var i = 0
    while (i < length) {
      if (!(bytes(offset + i) == value)) return false
      i += 1
    }
    true
  }

  // returns absolute offsets
  def equalOffsets(offset: Int, length: Int, value: Byte): Array[Int] = {
    val offsets = scala.collection.mutable.ArrayBuffer[Int]()
    var i = 0
    while (i < length) {
      if (bytes(offset + i) == value) offsets.append(offset + i)
      i += 1
    }
    offsets.toArray
  }

  def putInt(offset: Int, value: Int): Unit = write {
    mirror.putInt(offset, value)
  }
  def putLong(offset: Int, value: Long): Unit = write {
    mirror.putLong(offset, value)
  }
  def putBytes(offset: Int, value: Bytes): Unit = write {
    value.copyTo(bytes, offset)
  }
  def put(offset: Int, value: Array[Byte]): Unit = write {
    System.arraycopy(value, 0, bytes, offset, value.length)
  }

  def getByte(offset: Int): Byte = read {
    bytes(offset)
  }
  def getInt(offset: Int): Int = read {
    mirror.getInt(offset)
  }
  def getLong(offset: Int): Long = read {
    mirror.getLong(offset)
  }
  def get(offset: Int, length: Int): Array[Byte] = read {
    val copyBytes = Array.fill[Byte](length)(0)
    System.arraycopy(bytes, offset, copyBytes, 0, length)
    copyBytes
  }

  def extended(extendSize: Int, ro: Boolean): Memory = {
    read {
      val newBytes = Array.fill[Byte](bytes.length + extendSize)(0)
      System.arraycopy(bytes, 0, newBytes, 0, bytes.length)
      unloaded = true
      new Memory(newBytes, ro)
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
  def ofSize(size: Int, ro: Boolean): Memory = new Memory(Array.fill[Byte](size)(0), ro)
  def ofBytes(bytes: Array[Byte], ro: Boolean): Memory = new Memory(bytes.clone(), ro)
}

final class MemoryRef(val memory: Memory, val offset: Int) {
  def putInt(value: Int): Unit = memory.putInt(offset ,value)
  def putLong(value: Long): Unit = memory.putLong(offset, value)
  def put(value: Array[Byte]): Unit = memory.put(offset, value)

  def getInt(): Int = memory.getInt(offset)
  def getLong(): Long = memory.getLong(offset)
  def get(length: Int): Array[Byte] = memory.get(offset, length)
}