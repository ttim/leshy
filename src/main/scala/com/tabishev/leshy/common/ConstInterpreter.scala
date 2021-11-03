package com.tabishev.leshy.common

import com.tabishev.leshy.ast.{Address, Bytes, Const}
import com.tabishev.leshy.runtime.{FrameOffset, Symbol, Symbols}

trait ConstInterpreter {
  def frameSize(): Int
  def symbols(): Symbols

  def isConst(from: FrameOffset, length: Int): Boolean
  def get(from: FrameOffset, length: Int): Array[Byte]

  final def evalConst(const: Const): Bytes = const match {
    case Const.Literal(bytes) =>
      bytes
    case Const.Symbol(name) =>
      symbols().resolve(name).asBytes
    case Const.Stack(fromBytes, lengthBytes) =>
      val from = frameOffset(fromBytes.asInt)
      val length = lengthBytes.asInt
      assert(isConst(from, length))
      Bytes.fromBytes(get(from, length))
  }

  final def checkConst(constOrAddress: Const | Address, length: Int): Boolean =
    tryConst(constOrAddress, length).isDefined

  final def tryConst(constOrAddress: Const | Address, length: Int): Option[Bytes] = constOrAddress match {
    case const: Const =>
      Some(evalConst(const).expand(length))
    case Address.Native(_) =>
      None
    case Address.Stack(offsetAst) =>
      val offset = frameOffset(evalConst(offsetAst).asInt)
      if (isConst(offset, length)) {
        Some(Bytes.fromBytes(get(offset, length)))
      } else None
    case Address.StackOffset(_, _, _) =>
      ???
  }

  final def evalSymbol(const: Const): Symbol =
    symbols().resolveBytes(evalConst(const))

  final def frameOffset(rawOffset: Int): FrameOffset = FrameOffset.maybeNegative(rawOffset, frameSize())
}
