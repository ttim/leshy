package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.Bytes

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

final class Runtime {
  val stack = new StackMemory()
  val symbols = new Symbols()
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
  var stack: Memory = Memory.ofSize(10)
  var size: Int = 0
  var offset: Int = 0

  def getCurrentStackFrame(): Array[Byte] = stack.get(offset, size - offset)

  def getRef(index: Int): MemoryRef =
    if (index >= 0) {
      assert(index < (size - offset))
      new MemoryRef(stack, offset + index)
    } else {
      assert((-index) < (size - offset))
      new MemoryRef(stack, size + index)
    }

  def extend(extendSize: Int): Unit = {
    if (size + extendSize > stack.size) stack = stack.extended(Math.min(stack.size, extendSize))
    size += extendSize
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(size - offset >= shrinkSize)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def clean(): Unit = {
    size = 0
    offset = 0
  }

  def append(bytes: Bytes): Unit = {
    extend(bytes.length())
    stack.putBytes(size - bytes.length(), bytes)
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.offset)

  def offset(newOffset: Int): Unit = {
    assert(offset >= 0)
    this.offset = newOffset
  }

  override def toString: String = s"[${stack.get(offset, size - offset).mkString(", ")}]"
}