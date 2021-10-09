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
    case Operation.Extend(length) => state.extend(evalConstAsInt(length))
    case Operation.Shrink(length) => state.shrink(evalConstAsInt(length))
    case Operation.Append(bytes) => state.append(evalConst(bytes))
    case Operation.Call(offsetConst, targetConst) => {
      val offsetChange = evalConstAsInt(offsetConst)
      val target = evalConst(targetConst)
      state.advance(offsetChange)
      run(new String(target))
      state.retreat(offsetChange)
    }
    case Operation.CheckSize(length) => state.checkSize(evalConstAsInt(length))
    case Operation.Branch(modifier, length, op1, op2, target) => {
      val lengthE = evalConstAsInt(length)
      val op1E = evalConstOrAddress(lengthE, op1)
      val op2E = evalConstOrAddress(lengthE, op2)
      val modifierE = new String(evalConst(modifier))
      val flag = modifierE match {
        case "eq" => util.Arrays.equals(op1E, op2E)
        case "neq" => !util.Arrays.equals(op1E, op2E)
        case "le" => InterpreterUtils.arraysLess(op1E, op2E, orEqual = true)
        case "m" => !InterpreterUtils.arraysLess(op1E, op2E, orEqual = true)
        case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifierE''")
      }
      if (flag) run(new String(evalConst(target)))
    }
    case Operation.PrintInt(length, src) => {
      evalConstAsInt(length) match {
        case 4 => println(evalAddressAsInt(src))
        case 8 => println(evalAddressAsLong(src))
        case other => throw new IllegalArgumentException(s"unsupported int width for printing: $other")
      }
    }
    case Operation.Add(length, op1, op2, dst) => {
      evalConstAsInt(length) match {
        case 4 => putInt(dst, evalConstOrAddressAsInt(op1) + evalConstOrAddressAsInt(op2))
        case 8 => putLong(dst, evalConstOrAddressAsLong(op1) + evalConstOrAddressAsLong(op2))
        case other => throw new IllegalArgumentException(s"unsupported add length '$other'")
      }
    }
    case Operation.Copy(length, src, dst) => {
      // todo: to int seems wrong
      val lengthE = evalConstOrAddressAsLong(length).toInt
      val bytes = evalConstOrAddress(lengthE, src)
      put(dst, bytes)
    }
    case _ => throw new IllegalArgumentException(s"unsupported operation '$op''")
  }

  private def evalConstOrAddress(length: Int, constOrAddress: Const | Address): Array[Byte] = constOrAddress match {
    case const: Const => evalConst(const)
    case address: Address => evalAddress(length, address)
  }

  private def evalConstOrAddressAsInt(constOrAddress: Const | Address): Int = constOrAddress match {
    case const: Const => evalConstAsInt(const)
    case address: Address => evalAddressAsInt(address)
  }

  private def evalConstOrAddressAsLong(constOrAddress: Const | Address): Long = constOrAddress match {
    case const: Const => evalConstAsLong(const)
    case address: Address => evalAddressAsLong(address)
  }

  private def evalAddress(length: Int, address: Address): Array[Byte] = address match {
    case Address.DirectStack(address) => state.getStack(length, evalConstAsInt(address))
    case _ => throw new IllegalArgumentException(s"unsupported address: $address")
  }

  private def evalAddressAsInt(address: Address): Int =
    ByteBuffer.wrap(evalAddress(4, address)).getInt

  private def evalAddressAsLong(address: Address): Long =
    ByteBuffer.wrap(evalAddress(8, address)).getLong

  private def putInt(address: Address, value: Int): Unit = address match {
    case Address.DirectStack(address) => state.putStackInt(evalConstAsInt(address), value)
    case _ => throw new IllegalArgumentException(s"unsupported put int address: $address")
  }

  private def putLong(address: Address, value: Long): Unit = address match {
    case Address.DirectStack(address) => state.putStackLong(evalConstAsInt(address), value)
    case _ => throw new IllegalArgumentException(s"unsupported put int address: $address")
  }

  private def put(address: Address, value: Array[Byte]): Unit = address match {
    case Address.DirectStack(address) => state.putStack(evalConstAsInt(address), value)
    case _ => throw new IllegalArgumentException(s"unsupported put int address: $address")
  }

  private def evalConst(const: Const): Array[Byte] = const match {
    case Const.Literal(bytes) => bytes
    case Const.Stack(from, length) => ???
  }

  private def evalConstAsInt(const: Const): Int = {
    val bytes = evalConst(const)
    assert(bytes.length == 4)
    ByteBuffer.wrap(bytes).getInt
  }

  private def evalConstAsLong(const: Const): Long = {
    val bytes = evalConst(const)
    if (bytes.length == 8) {
      ByteBuffer.wrap(bytes).getLong
    } else if (bytes.length < 8) {
      val toConvert = Array.fill[Byte](8)(0)
      System.arraycopy(bytes, 0, toConvert, 0, bytes.length)
      // todo: not correct I think when < 0?
      ByteBuffer.wrap(toConvert).getLong
    } else throw new IllegalArgumentException("const has more than 8 bytes")
  }
}
