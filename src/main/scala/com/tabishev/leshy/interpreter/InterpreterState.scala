package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.Bytes

import java.nio.ByteBuffer
import scala.collection.mutable

class InterpreterState {
  val stack = new StackMemory()

  private val symbolsByName: mutable.Map[String, Symbol] = mutable.HashMap()
  private val symbolsById: mutable.Map[Int, Symbol] = mutable.HashMap()
  private var nextSymbol: Int = 1

  def registerSymbol(name: String): Symbol =
    if (symbolsByName.contains(name)) symbolsByName(name) else {
      val symbol = Symbol(name, nextSymbol)
      nextSymbol += 1

      symbolsByName.put(name, symbol)
      symbolsById.put(symbol.id, symbol)

      symbol
    }

  def resolveSymbol(name: String): Symbol = symbolsByName(name)
  def resolveSymbolBytes(bytes: Bytes): Symbol = symbolsById(bytes.asInt.get)

  def getStack(length: Int, address: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    System.arraycopy(stack.stack, stack.offset + address, bytes, 0, length)
    bytes
  }

  def getStack(): Array[Byte] = getStack(stack.size - stack.offset, 0)
}

class StackMemory {
  var stack: Array[Byte] = Array.fill(10)(0)
  var mirror: ByteBuffer = ByteBuffer.wrap(stack)
  var size: Int = 0
  var offset: Int = 0

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
      mirror = ByteBuffer.wrap(stack)
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