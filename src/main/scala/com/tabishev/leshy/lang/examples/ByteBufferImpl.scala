package com.tabishev.leshy.lang.examples

import java.nio.{ByteBuffer, ByteOrder}

object ByteBufferImpl {
  private val buffer: ByteBuffer = {
    val bb = ByteBuffer.allocate(24)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb
  }

  def ffactorial4(n: Int): Int = {
    buffer.putInt(0, n) // n
    buffer.putInt(4, n) // i
    buffer.putInt(8, 1) // ans

    while (buffer.getInt(4) > 0) {
      buffer.putInt(8, buffer.getInt(8) * buffer.getInt(4)) // ans = ans * i
      buffer.putInt(4, buffer.getInt(4) - 2) // i = i - 2
    }

    buffer.getInt(8)
  }

  def ffactorial8(n: Int): Long = {
    buffer.putLong(0, n) // n
    buffer.putLong(8, n) // i
    buffer.putLong(16, 1) // ans

    while (buffer.getLong(8) > 0) {
      buffer.putLong(16, buffer.getLong(16) * buffer.getLong(8)) // ans = ans * i
      buffer.putLong(8, buffer.getLong(8) - 2) // i = i - 2
    }

    buffer.getLong(16)
  }
}
