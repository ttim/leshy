package com.tabishev.leshy.examples

import java.nio.{ByteBuffer, ByteOrder}

object ByteBufferImpl {
  def ffactorial4(n: Int): Int = {
    val bb = ByteBuffer.allocate(12)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(0, n) // n
    bb.putInt(4, n) // i
    bb.putInt(8, 1) // ans

    while (bb.getInt(4) > 0) {
      bb.putInt(8, bb.getInt(8) * bb.getInt(4)) // ans = ans * i
      bb.putInt(4, bb.getInt(4) - 2) // i = i - 2
    }

    bb.getInt(8)
  }

  def ffactorial8(n: Int): Long = {
    val bb = ByteBuffer.allocate(24)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putLong(0, n) // n
    bb.putLong(8, n) // i
    bb.putLong(16, 1) // ans

    while (bb.getLong(8) > 0) {
      bb.putLong(16, bb.getLong(16) * bb.getLong(8)) // ans = ans * i
      bb.putLong(8, bb.getLong(8) - 2) // i = i - 2
    }

    bb.getLong(16)
  }
}
