package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

class Runtime {
  val stack = new StackMemory()
  val symbols = new Symbols()
}

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

case class Symbol(name: String, id: Int) {
  val asBytes: Bytes = Bytes.fromInt(id)
}

class Symbols {
  private val symbolsByName: mutable.Map[String, Symbol] = mutable.HashMap()
  private val symbolsById: mutable.Map[Int, Symbol] = mutable.HashMap()
  private var nextSymbol: Int = 1

  def register(name: String): Symbol =
    if (symbolsByName.contains(name)) symbolsByName(name) else {
      val symbol = Symbol(name, nextSymbol)
      nextSymbol += 1

      symbolsByName.put(name, symbol)
      symbolsById.put(symbol.id, symbol)

      symbol
    }

  def resolve(name: String): Symbol = symbolsByName(name)
  def resolveBytes(bytes: Bytes): Symbol = symbolsById(bytes.asInt.get)
}

class StackMemory {
  var stack: Array[Byte] = Array.fill(10)(0)
  var mirror: ByteBuffer = ByteBuffer.wrap(stack).order(ByteOrder.LITTLE_ENDIAN)
  var size: Int = 0
  var offset: Int = 0

  def get(length: Int, address: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    System.arraycopy(stack, offset + address, bytes, 0, length)
    bytes
  }

  def getFully(): Array[Byte] = get(size - offset, 0)

  def getRef(index: Int): MemoryRef =
    if (index >= 0) {
      assert(index < (size - offset))
      new MemoryRef(mirror, offset + index)
    } else {
      assert((-index) < (size - offset))
      new MemoryRef(mirror, size + index)
    }

  def extend(extendSize: Int): Unit = {
    if (size + extendSize <= stack.length) size += extendSize else {
      val newStack = Array.fill[Byte](stack.length * 2)(0)
      System.arraycopy(stack, 0, newStack, 0, stack.length)
      stack = newStack
      mirror = ByteBuffer.wrap(stack).order(ByteOrder.LITTLE_ENDIAN)
      extend(extendSize)
    }
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(size - offset >= shrinkSize)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def append(bytes: Bytes): Unit = {
    extend(bytes.length())
    bytes.copyTo(stack, size - bytes.length())
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.offset)

  def offset(newOffset: Int): Unit = {
    assert(offset >= 0)
    this.offset = newOffset
  }

  override def toString: String = s"[${stack.slice(offset, size).mkString(", ")}]"
}