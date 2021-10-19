package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}

// todo: use MemoryAddress, MemorySegment & MemoryAccess instead?
final class Memory private (val bytes: Array[Byte]) {
  private val mirror = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  def size: Int = bytes.length

  def putInt(offset: Int, value: Int): Unit = mirror.putInt(offset, value)
  def putLong(offset: Int, value: Long): Unit = mirror.putLong(offset, value)
  def putBytes(offset: Int, value: Bytes): Unit = value.copyTo(bytes, offset)
  def put(offset: Int, value: Array[Byte]): Unit =
    System.arraycopy(value, 0, bytes, offset, value.length)

  def getInt(offset: Int): Int = mirror.getInt(offset)
  def getLong(offset: Int): Long = mirror.getLong(offset)
  def get(offset: Int, length: Int): Array[Byte] = {
    val copyBytes = Array.fill[Byte](length)(0)
    System.arraycopy(bytes, offset, copyBytes, 0, length)
    copyBytes
  }

  def extended(extendSize: Int): Memory = {
    val newBytes = Array.fill[Byte](bytes.length + extendSize)(0)
    System.arraycopy(bytes, 0, newBytes, 0, bytes.length)
    new Memory(newBytes)
  }
}

object Memory {
  def ofSize(size: Int): Memory = new Memory(Array.fill[Byte](size)(0))
  def ofBytes(bytes: Array[Byte]): Memory = new Memory(bytes.clone())
}

final class MemoryRef(val memory: Memory, val offset: Int) {
  def putInt(value: Int): Unit = memory.putInt(offset ,value)
  def putLong(value: Long): Unit = memory.putLong(offset, value)
  def put(value: Array[Byte]): Unit = memory.put(offset, value)

  def getInt(): Int = memory.getInt(offset)
  def getLong(): Long = memory.getLong(offset)
  def get(length: Int): Array[Byte] = memory.get(offset, length)
}