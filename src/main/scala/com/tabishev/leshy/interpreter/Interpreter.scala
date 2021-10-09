package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Const, Operation, Subroutine}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File
import java.nio.ByteBuffer
import java.util

object Interpreter {
  def run(loader: RoutineLoader, name: String): Unit =
    new InterpreterSession(loader).run(name)

  def main(args: Array[String]): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/fib.lsh").toPath)
    run(loader, "main")
  }
}

private class InterpreterSession(loader: RoutineLoader) {
  private val state = new InterpreterState()

  def run(name: String): Unit = run(loader.load(name).get)

  private def run(subroutine: Subroutine): Unit = {
    subroutine.ops.foreach(run)
  }

  private def run(op: Operation): Unit = op match {
    case Operation.Extend(length) =>
      state.stack.extend(constRef(length, 4).getInt())
    case Operation.Shrink(length) =>
      state.stack.shrink(constRef(length, 4).getInt())
    case Operation.Append(bytes) =>
      state.stack.append(evalConst(bytes))
    case Operation.Call(offsetConst, targetConst) => {
      val offsetChange = constRef(offsetConst, 4).getInt()
      val target = evalConst(targetConst)
      state.stack.advance(offsetChange)
      run(new String(target))
      state.stack.retreat(offsetChange)
    }
    case Operation.CheckSize(length) =>
      state.stack.checkSize(constRef(length, 4).getInt())
    case Operation.Branch(modifier, length, op1, op2, target) => {
      val lengthE = constRef(length, 4).getInt()
      val modifierE = new String(evalConst(modifier))
      val op1Ref = constOrAddressRef(op1, lengthE)
      val op2Ref = constOrAddressRef(op2, lengthE)
      val flag = modifierE match {
        case "eq" => Runtime.equals(op1Ref, op2Ref)
        case "neq" => !Runtime.equals(op1Ref, op2Ref)
        case "le" => Runtime.arraysLess(lengthE, op1Ref, op2Ref, orEqual = true)
        case "m" => !Runtime.arraysLess(lengthE, op1Ref, op2Ref, orEqual = true)
        case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifierE''")
      }
      if (flag) run(new String(evalConst(target)))
    }
    case Operation.PrintInt(length, src) =>
      Runtime.printInt(constRef(length, 4).getInt(), addressRef(src))
    case Operation.Add(length, op1, op2, dst) => {
      val lengthE = constRef(length, 4).getInt()
      Runtime.add(lengthE, constOrAddressRef(op1, lengthE), constOrAddressRef(op2, lengthE), addressRef(dst))
    }
    case Operation.Copy(length, src, dst) => {
      // todo: should be getLong
      val lengthE = constOrAddressRef(length, 4).getInt()
      Runtime.copy(lengthE, constOrAddressRef(src, lengthE), constOrAddressRef(dst, lengthE))
    }
    case _ =>
      throw new IllegalArgumentException(s"unsupported operation '$op''")
  }

  // refs
  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef = constOrAddress match {
    case const: Const => constRef(const, constExpectedLength)
    case address: Address => addressRef(address)
  }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.DirectStack(address) => state.stack.getRef(constRef(address, 4).getInt())
    case _ => throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const evaluation
  private def evalConst(const: Const): Array[Byte] = const match {
    case Const.Literal(bytes) => bytes
    case Const.Stack(from, length) => ???
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
