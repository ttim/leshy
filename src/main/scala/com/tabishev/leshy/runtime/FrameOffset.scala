package com.tabishev.leshy.runtime

case class FrameOffset private (get: Int) {
  def plus(other: FrameOffset): FrameOffset = FrameOffset(get + other.get)
}

object FrameOffset {
  val Zero: FrameOffset = FrameOffset(0)

  def nonNegative(offset: Int): FrameOffset = {
    assert(offset >= 0)
    FrameOffset(offset)
  }

  def maybeNegative(offset: Int, frameSize: Int): FrameOffset =
    if (offset >= 0) nonNegative(offset) else nonNegative(frameSize + offset)
}