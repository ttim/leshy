package com.tabishev.leshy.runtime

case class FrameOffset private (get: Int)

object FrameOffset {
  val Zero: FrameOffset = FrameOffset(0)

  def nonNegative(offset: Int): FrameOffset = {
    assert(offset >= 0)
    FrameOffset(offset)
  }

  def maybeNegative(offset: Int, frameSize: Int): FrameOffset = {
    if (offset >= 0) FrameOffset(offset) else FrameOffset(frameSize + offset)
  }
}