package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Const, Operation, Fn}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File
import java.nio.ByteBuffer
import java.util

object Interpreter {
  def run(loader: RoutineLoader, name: String, debug: Boolean): Unit =
    new InterpreterSession(loader, debug).run(name)
}

class InterpreterSession(loader: RoutineLoader, debug: Boolean) {
  private val state = new InterpreterState()

  def run(name: String): Unit = {
    assert(state.stack.size == 0)
    run(name, 0)
  }

  private def run(name: String, depth: Int): Unit =
    run(loader.load(name).get, depth)

  private def run(subroutine: Fn, depth: Int): Unit =
    subroutine.ops.foreach { op => run(op, depth) }

  private def run(op: Operation, depth: Int): Unit = {
    if (debug) println("\t".repeat(depth) + s"$op with ${state.stack}")
    op match {
      case Operation.Extend(length) =>
        state.stack.extend(constRef(length, 4).getInt())
      case Operation.Shrink(length) =>
        state.stack.shrink(constRef(length, 4).getInt())
      case Operation.Append(bytes) =>
        state.stack.append(evalConst(bytes))
      case Operation.Call(offsetConst, targetConst) => {
        val offsetChange = constRef(offsetConst, 4).getInt()
        val target = evalConst(targetConst)

        val prevOffset = state.stack.offset
        val newOffset = if (offsetChange >= 0) state.stack.offset + offsetChange else state.stack.size + offsetChange
        state.stack.offset(newOffset)
        // todo: that's where we increase stack depth even if it's regular for loop
        run(new String(target), depth + 1)
        state.stack.offset(prevOffset)
      }
      case Operation.CheckSize(length) =>
        state.stack.checkSize(constRef(length, 4).getInt())
      case Operation.Branch(modifier, length, op1, op2, target) => {
        val lengthE = constRef(length, 4).getInt()
        val modifierE = new String(evalConst(modifier))
        val op1Ref = constOrAddressRef(op1, lengthE)
        val op2Ref = constOrAddressRef(op2, lengthE)
        val flag = modifierE match {
          case "eq" => Runtime.arraysEquals(lengthE, op1Ref, op2Ref)
          case "neq" => !Runtime.arraysEquals(lengthE, op1Ref, op2Ref)
          case "le" => Runtime.arraysLess(lengthE, op1Ref, op2Ref, orEqual = true)
          case "m" => !Runtime.arraysLess(lengthE, op1Ref, op2Ref, orEqual = true)
          case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifierE''")
        }
        if (flag) run(new String(evalConst(target)), depth + 1)
      }
      case Operation.PrintInt(length, src) =>
        Runtime.printInt(constRef(length, 4).getInt(), addressRef(src))
      case Operation.Add(length, op1, op2, dst) => {
        val lengthE = constRef(length, 4).getInt()
        Runtime.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
      }
      case Operation.Mult(length, op1, op2, dst) => {
        val lengthE = constRef(length, 4).getInt()
        Runtime.mult(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
      }
      case Operation.Neg(length, op, dst) => {
        val lengthE = constRef(length, 4).getInt()
        Runtime.neg(lengthE, constOrAddressRef(op, lengthE), addressRef(dst))
      }
      case Operation.Copy(length, src, dst) => {
        // todo: should be getLong
        val lengthE = constOrAddressRef(length, 8).getLong()
        Runtime.copy(lengthE, constOrAddressRef(src, lengthE.toInt), constOrAddressRef(dst, lengthE.toInt))
      }
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  // refs
  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef = constOrAddress match {
    case const: Const => constRef(const, constExpectedLength)
    case address: Address => addressRef(address)
  }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.Stack(address, limit) => {
      // todo: do not ignore limit
      state.stack.getRef(constRef(address, 4).getInt())
    }
    case _ => throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const evaluation
  private def evalConst(const: Const): Array[Byte] = const match {
    case Const.Literal(bytes) => bytes.get()
    case Const.Stack(from, length) => {
      // todo: check constantness
      addressRef(Address.Stack(Const.Literal(from), Const.Literal(length))).getBytes(length.asInt.get)
    }
  }

  private def constRef(const: Const, expectedLength: Int): MemoryRef =
    new MemoryRef(ByteBuffer.wrap(expandedBytes(evalConst(const), expectedLength)), 0)
  private def expandedBytes(bytes: Array[Byte], expectedLength: Int): Array[Byte] =
    if (bytes.length == expectedLength) bytes else {
      val result = Array.fill[Byte](expectedLength)(0)
      System.arraycopy(bytes, 0, result, 0, bytes.length)
      result
    }
}
