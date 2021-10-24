package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.*

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

final class Runtime {
  val stack = new StackMemory()
  val symbols = new Symbols()

  CommonSymbols.register(symbols)
}

case class Consts private[runtime] (constOffsetsAndData: Bytes) {
  override def toString: String = {
    val offsetsWithValues = (0 until length).map { index =>
      val offset = constOffsetsAndData.asByteBuffer.getInt(index * 5)
      val value = constOffsetsAndData.asByteBuffer.get(index * 5 + 4)
      (offset, value)
    }
    s"[${offsetsWithValues.map(_._1).mkString(", ")}] -> [${offsetsWithValues.map(_._2).mkString(", ")}]"
  }

  def length: Int = constOffsetsAndData.length() / 5
}

class StackMemory {
  private val initialSize = 10

  var memory: Memory = Memory.ofSize(initialSize, ro = false)
  var marks: Memory = Memory.ofSize(initialSize, ro = false)

  var size: Int = 0
  var frameOffset: Int = 0

  def stackFrameSize(): Int = size - frameOffset
  def getCurrentStackFrame(): Array[Byte] = memory.get(frameOffset, size - frameOffset)

  private def calcAbsoluteOffset(offset: Int): Int =
    if (offset >= 0) {
      assert(offset < (size - frameOffset))
      frameOffset + offset
    } else {
      assert((-offset) <= (size - frameOffset))
      size + offset
    }

  def getRef(offset: Int): MemoryRef = new MemoryRef(memory, calcAbsoluteOffset(offset))

  def extend(extendSize: Int): Unit = {
    assert(extendSize >= 0)
    if (size + extendSize > memory.size) {
      val memoryExtendSize = Math.min(memory.size, extendSize)
      memory = memory.extended(memoryExtendSize, ro = false)
      marks = marks.extended(memoryExtendSize, ro = false)
    }
    size += extendSize
    markConst(-extendSize, extendSize, isConst = true)
  }

  def shrink(shrinkSize: Int): Unit = {
    assert(shrinkSize >= 0)
    assert(size - frameOffset >= shrinkSize)
    memory.fill(size - shrinkSize, shrinkSize, 0)
    markConst(-shrinkSize, shrinkSize, isConst = false)
    size -= shrinkSize
    // todo: decrease size in some cases?
  }

  def clean(): Unit = {
    size = 0
    frameOffset = 0
  }

  def append(bytes: Bytes, isConst: Boolean): Unit = {
    extend(bytes.length())
    memory.putBytes(size - bytes.length(), bytes)
    markConst(size - bytes.length(), bytes.length(), isConst)
  }

  def checkSize(size: Int): Unit = assert(size == this.size - this.frameOffset)

  def offset(newOffset: Int): Unit = {
    assert(frameOffset >= 0)
    this.frameOffset = newOffset
  }

  def frameToString: String = {
    import scala.io.AnsiColor.RED
    import scala.io.AnsiColor.RESET

    val frameData = memory.get(frameOffset, size - frameOffset)
    val frameMarks = marks.get(frameOffset, size - frameOffset)
    frameData.zip(frameMarks).map { case (byte, mark) =>
      if (mark == 0) byte.toString else RED + byte.toString + RESET
    }.mkString(", ")
  }

  // const actions
  def markConst(offset: Int, length: Int, isConst: Boolean): Unit = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.fill(absoluteOffset, length, if (isConst) 1 else 0)
  }

  def isConst(offset: Int, length: Int): Boolean = {
    val absoluteOffset = calcAbsoluteOffset(offset)
    assert(absoluteOffset + length <= size)
    marks.allEquals(absoluteOffset, length, 1)
  }

  def stackFrameConsts(): Consts = {
    val constOffsets = marks.equalOffsets(frameOffset, size - frameOffset, 1)
    val offsetsAndData = Array.fill[Byte](constOffsets.length * 5)(0)
    val mirror = ByteBuffer.wrap(offsetsAndData).order(ByteOrder.LITTLE_ENDIAN)
    constOffsets.zipWithIndex.foreach { case (offset, index) =>
      mirror.putInt(index * 5, offset - frameOffset)
      mirror.put(index * 5 + 4, memory.getByte(offset))
    }
    Consts(Bytes.fromBytes(offsetsAndData))
  }
}