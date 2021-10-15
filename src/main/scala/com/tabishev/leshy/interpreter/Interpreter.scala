package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File
import java.nio.ByteBuffer
import java.util

class Interpreter(loader: RoutineLoader, debug: Boolean) {
  private val state = new InterpreterState()

  def run(name: String, input: Bytes): Bytes = {
    assert(state.stack.size == 0)
    state.stack.append(input)
    try {
      run(name, 0)
      assert(state.stack.offset == 0)
      Bytes.fromBytes(state.getStack())
    } finally {
      state.stack.shrink(state.stack.size)
    }
  }

  private def run(name: String, depth: Int): Unit = {
    var subroutine = loader.load(name).getOrElse(throw new IllegalArgumentException(s"can't load $name"))
    var i = 0
    while (i < subroutine.ops.length) {
      run(subroutine.ops(i), depth) match {
        case Some(toJump) =>
          i = subroutine.labels(toJump)
        case None =>
          i += 1
      }
    }
  }

  private def run(op: Operation, depth: Int): Option[String] = {
    if (debug) println("\t".repeat(depth) + s"$op with ${state.stack}")
    op match {
      case Operation.Extend(length) =>
        state.stack.extend(evalConst(length).asExpandedInt.get)
        None
      case Operation.Shrink(length) =>
        state.stack.shrink(evalConst(length).asExpandedInt.get)
        None
      case Operation.Append(bytes) =>
        state.stack.append(evalConst(bytes))
        None
      case Operation.Call(offsetConst, targetConst) => {
        val offsetChange = evalConst(offsetConst).asExpandedInt.get
        val target = evalConst(targetConst)

        val prevOffset = state.stack.offset
        val newOffset = if (offsetChange >= 0) state.stack.offset + offsetChange else state.stack.size + offsetChange
        state.stack.offset(newOffset)
        // todo: that's where we increase stack depth even if it's regular for loop
        run(target.asString.get, depth + 1)
        state.stack.offset(prevOffset)
        None
      }
      case Operation.CheckSize(length) =>
        state.stack.checkSize(evalConst(length).asExpandedInt.get)
        None
      case Operation.Branch(modifier, length, op1, op2, target) => {
        val lengthE = evalConst(length).asExpandedInt.get
        val modifierE = evalConst(modifier).asString.get
        val op1Ref = constOrAddressRef(op1, lengthE)
        val op2Ref = constOrAddressRef(op2, lengthE)
        val flag = modifierE match {
          case "eq" => Runtime.equals(lengthE, op1Ref, op2Ref)
          case "neq" => !Runtime.equals(lengthE, op1Ref, op2Ref)
          case "le" => Runtime.less(lengthE, op1Ref, op2Ref, orEqual = true)
          case "m" => !Runtime.less(lengthE, op1Ref, op2Ref, orEqual = true)
          case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifierE''")
        }
        if (flag) Some(evalConst(target).asString.get) else None
      }
      case Operation.Jump(target) =>
        Some(evalConst(target).asString.get)
      case Operation.PrintInt(length, src) =>
        Runtime.printInt(evalConst(length).asExpandedInt.get, addressRef(src))
        None
      case Operation.Add(length, op1, op2, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        Runtime.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        None
      }
      case Operation.Mult(length, op1, op2, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        Runtime.mult(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
        None
      }
      case Operation.Neg(length, op, dst) => {
        val lengthE = evalConst(length).asExpandedInt.get
        Runtime.neg(lengthE, constOrAddressRef(op, lengthE), addressRef(dst))
        None
      }
      case Operation.Set(length, src, dst) => {
        val lengthE = evalConst(length).asExpandedLong.get
        Runtime.set(lengthE, constOrAddressRef(src, lengthE.toInt), constOrAddressRef(dst, lengthE.toInt))
        None
      }
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef =
    constOrAddress match {
      case const: Const =>
        new MemoryRef(evalConst(const).expand(constExpectedLength).asByteBuffer, 0)
      case address: Address =>
        addressRef(address)
    }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.Stack(address) =>
      state.stack.getRef(evalConst(address).asExpandedInt.get)
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const evaluation
  private def evalConst(const: Const): Bytes = const match {
    case Const.Literal(bytes) => bytes
    case Const.Stack(from, length) => {
      // todo: check constantness
      val address = addressRef(Address.Stack(Const.Literal(from)))
      Bytes.fromBytes(address.getBytes(length.asInt.get))
    }
  }
}
