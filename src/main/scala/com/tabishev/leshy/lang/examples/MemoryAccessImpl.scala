package com.tabishev.leshy.lang.examples

import jdk.incubator.foreign.{MemoryAccess, MemoryAddress, MemorySegment, ResourceScope}

import java.nio.{ByteBuffer, ByteOrder}

object MemoryAccessImpl {
  private val native = true
  private val memory: MemorySegment =
    if (native) MemorySegment.allocateNative(24, ResourceScope.globalScope())
    else MemorySegment.ofArray(Array.fill[Byte](24)(0))

  def ffactorial4(n: Int): Int = {
    MemoryAccess.setIntAtOffset(memory, 0, n) // n
    MemoryAccess.setIntAtOffset(memory, 4, n) // i
    MemoryAccess.setIntAtOffset(memory, 8, 1) // ans

    while (MemoryAccess.getIntAtOffset(memory, 4) > 0) {
      MemoryAccess.setIntAtOffset(memory, 8, // ans =
        MemoryAccess.getIntAtOffset(memory, 8)*MemoryAccess.getIntAtOffset(memory, 4)) // ans*i
      MemoryAccess.setIntAtOffset(memory, 4, // i =
        MemoryAccess.getIntAtOffset(memory, 4) - 2) // i-2
    }

    MemoryAccess.getIntAtOffset(memory, 8)
  }

  def ffactorial8(n: Int): Long = {
    MemoryAccess.setLongAtOffset(memory, 0, n) // n
    MemoryAccess.setLongAtOffset(memory, 8, n) // i
    MemoryAccess.setLongAtOffset(memory, 16, 1) // ans

    while (MemoryAccess.getLongAtOffset(memory, 8) > 0) {
      MemoryAccess.setLongAtOffset(memory, 16, // ans =
        MemoryAccess.getLongAtOffset(memory, 16)*MemoryAccess.getLongAtOffset(memory, 8)) // ans*i
      MemoryAccess.setLongAtOffset(memory, 8, // i =
        MemoryAccess.getLongAtOffset(memory, 8) - 2) // i-2
    }

    MemoryAccess.getLongAtOffset(memory, 16)
  }
}
