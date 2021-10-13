package com.tabishev.leshy.interpreter

import java.nio.{ByteBuffer, ByteOrder}

// todo: use MemoryAddress, MemorySegment & MemoryAccess instead?
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
  // todo: should be flag for unsigned/signed comparison
  // todo: add arbitrary version
  def less(length: Int, arg1: MemoryRef, arg2: MemoryRef, orEqual: Boolean): Boolean = length match {
    case 4 => less4(arg1, arg2, orEqual)
    case 8 => less8(arg1, arg2, orEqual)
    case _ => throw new IllegalArgumentException(s"unsupported 'less' length $length")
  }

  def less4(arg1: MemoryRef, arg2: MemoryRef, orEqual: Boolean): Boolean =
    if (orEqual) (arg1.getInt() <= arg2.getInt()) else (arg1.getInt() < arg2.getInt())

  def less8(arg1: MemoryRef, arg2: MemoryRef, orEqual: Boolean): Boolean =
    if (orEqual) (arg1.getLong() <= arg2.getLong()) else (arg1.getLong() < arg2.getLong())

  // todo: add arbitrary version
  def equals(length: Int, arg1: MemoryRef, arg2: MemoryRef): Boolean = length match {
    case 4 => equals4(arg1, arg2)
    case 8 => equals8(arg1, arg2)
    case _ => throw new IllegalArgumentException(s"unsupported 'equals' length $length")
  }

  def equals4(arg1: MemoryRef, arg2: MemoryRef): Boolean = arg1.getInt() == arg2.getInt()

  def equals8(arg1: MemoryRef, arg2: MemoryRef): Boolean = arg1.getLong() == arg2.getLong()

  def add(length: Int, arg1: MemoryRef, arg2: MemoryRef, dst: MemoryRef): Unit = length match {
    case 4 => dst.putInt(arg1.getInt() + arg2.getInt())
    case 8 => dst.putLong(arg1.getLong() + arg2.getLong())
    case other => throw new IllegalArgumentException(s"unsupported add length '$other'")
  }

  def mult(length: Int, arg1: MemoryRef, arg2: MemoryRef, dst: MemoryRef): Unit = length match {
    case 4 => dst.putInt(arg1.getInt() * arg2.getInt())
    case 8 => dst.putLong(arg1.getLong() * arg2.getLong())
    case other => throw new IllegalArgumentException(s"unsupported mult length '$other'")
  }

  def neg(length: Int, arg: MemoryRef, dst: MemoryRef): Unit = length match {
    case 4 => dst.putInt(-arg.getInt())
    case 8 => dst.putLong(-arg.getLong())
    case other => throw new IllegalArgumentException(s"unsupported neg length '$other'")
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
