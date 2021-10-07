package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Const, Operation, Subroutine}
import com.tabishev.leshy.loader.{FileLoader, RoutineLoader}

import java.io.File
import java.nio.ByteBuffer

object Interpreter {
  def run(loader: RoutineLoader, name: String): Unit =
    new InterpreterSession(loader).run(name)

  def main(args: Array[String]): Unit = {
    val loader = FileLoader.fromFile(new File("src/main/lsh/fib.lsh").toPath)
    run(loader, "main")
  }
}

private class InterpreterSession(loader: RoutineLoader) {
  var stack: Array[Byte] = Array.fill(10)(0)
  var mirror: ByteBuffer = ByteBuffer.wrap(stack)
  var size: Int = 0
  var offset: Int = 0

  def run(name: String): Unit = run(loader.load(name).get)

  private def run(subroutine: Subroutine): Unit = {
    subroutine.ops.foreach(run)
  }

  private def run(op: Operation): Unit = op match {
    case Operation.Extend(length) => ???
    case Operation.Append(bytes) => {
      val evaled = evalConst(bytes)
      extend(evaled.length)
      System.arraycopy(evaled, 0, stack, size - evaled.length, evaled.length)
    }
    case Operation.Call(offsetConst, targetConst) => {
      val offsetChange = evalConstAsInt(offsetConst)
      offset += offsetChange
      val target = evalConst(targetConst)
      run(new String(target))
      offset -= offsetChange
    }
    case Operation.CheckSize(length) => {
      assert(evalConstAsInt(length) == size - offset)
    }
    case Operation.Branch(modifier, length, op1, op2, target) => {
      val lengthEvaled = evalConstAsInt(length)
      assert(lengthEvaled == 4)
      ???
    }
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

  private def extend(extendSize: Int): Unit = {
    if (size + extendSize <= stack.length) size += extendSize else {
      val newStack = Array.fill[Byte](stack.length * 2)(0)
      System.arraycopy(stack, 0, newStack, 0, stack.length)
      stack = newStack
      mirror = ByteBuffer.wrap(stack)
      extend(extendSize)
    }
  }
}