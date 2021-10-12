package com.tabishev.leshy.interpreter

import java.nio.{ByteBuffer, ByteOrder}

class MemoryRef(val buffer: ByteBuffer, val index: Int) {
  buffer.order(ByteOrder.LITTLE_ENDIAN)

  def putInt(value: Int): Unit = buffer.putInt(index, value)
  def putLong(value: Long): Unit = buffer.putLong(index, value)
  def put(value: Array[Byte]): Unit = buffer.put(index, value)

  def getInt(): Int = buffer.getInt(index)
  def getLong(): Long = buffer.getLong(index)
  def getByte(offset: Int): Byte = buffer.get(index + offset)
  def getBytes(length: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    buffer.get(index, bytes, 0, length)
    bytes
  }
}

object Runtime {
  def arraysLess(length: Int, arg1: MemoryRef, arg2: MemoryRef, orEqual: Boolean): Boolean = {
    var i = 0
    while (i < length) {
      val b1 = arg1.getByte(i)
      val b2 = arg2.getByte(i)
      if (b1 < b2) return true
      if (b1 > b2) return false
      i += 1
    }
    orEqual
  }

  def arraysEqual(length: Int, arg1: MemoryRef, arg2: MemoryRef): Boolean = {
    var i = 0
    while (i < length) {
      if (arg1.getByte(i) != arg2.getByte(i)) return false
      i += 1
    }
    true
  }

  def add(length: Int, arg1: MemoryRef, arg2: MemoryRef, dst: MemoryRef): Unit = length match {
    case 4 => dst.putInt(arg1.getInt() + arg2.getInt())
    case 8 => dst.putLong(arg1.getLong() + arg2.getLong())
    case other => throw new IllegalArgumentException(s"unsupported add length '$other'")
  }

  def copy(length: Long, src: MemoryRef, dest: MemoryRef): Unit = {
    // todo: do manipulation over underlying byte buffers!
    dest.put(src.getBytes(length.toInt))
  }

  def printInt(length: Int, arg: MemoryRef): Unit = length match {
    case 4 => println(arg.getInt())
    case 8 => println(arg.getLong())
    case other => throw new IllegalArgumentException(s"unsupported int width for printing: $other")
  }
}
