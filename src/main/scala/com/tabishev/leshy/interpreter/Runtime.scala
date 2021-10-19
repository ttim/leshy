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
  var memory: Memory = Memory.ofSize(10, ro = false)
  var size: Int = 0
  var offset: Int = 0

  def getCurrentStackFrame(): Array[Byte] = memory.get(offset, size - offset)

  def getRef(index: Int): MemoryRef =
    if (index >= 0) {
      assert(index < (size - offset))
      new MemoryRef(memory, offset + index)
    } else {
      assert((-index) < (size - offset))
      new MemoryRef(memory, size + index)
    }

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    if (size + extendSize > memory.size) memory = memory.extended(Math.min(memory.size, extendSize), ro = false)
    size += extendSize
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    assert(size - offset >= shrinkSize)
    memory.zero(size - shrinkSize, shrinkSize)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def clean(): Unit = {
    size = 0
    offset = 0
  }

  def append(bytes: Bytes): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.offset)

  def offset(newOffset: Int): Unit = {
    assert(offset >= 0)
    this.offset = newOffset
  }

  override def toString: String = s"[${memory.get(offset, size - offset).mkString(", ")}]"
}