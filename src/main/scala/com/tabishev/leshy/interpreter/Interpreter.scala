package com.tabishev.leshy.interpreter

import com.tabishev.leshy.ast.{Address, Bytes, Const, Fn, Operation, OperationWithSource}
import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.loader.{FileLoader, FnLoader}
import com.tabishev.leshy.runtime._

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: FnLoader, debug: Boolean, checkConsts: Boolean) extends ConstInterpreter {
  private val runtime = new Runtime()
  private var consts = Consts.Empty

  def run[T, V](spec: FnSpec[T, V])(input: T): V = {
    val inputObj = spec.input(input)

    assert(runtime.stack.isEmpty())
    runtime.stack.append(inputObj.bytes)
    updateConsts(_ => inputObj.consts)
    try {
      run(spec.fn, 0)
      assert(runtime.stack.getFrameOffset() == 0)
      spec.output(Bytes.fromBytes(runtime.stack.currentStackFrame()))
    } finally {
      runtime.stack.clean()
      updateConsts(_ => Consts.Empty)
    }
  }

  // for rest
  private def run(name: String, depth: Int): Unit = {
    val fn = loader.load(name).get
    runtime.symbols.register(fn)

    var i = 0
    while (i < fn.ops.length) {
      run(fn.ops(i), depth) match {
        case Some(toJump) =>
          i = fn.labels(toJump.name)
        case None =>
          i += 1
      }
    }
  }

  private def run(op: OperationWithSource, depth: Int): Option[Symbol] = {
    if (debug) println("\t".repeat(depth) + s"${runtime.stack.frameToString(consts)}: ${op.op}")
    op.op match {
      case Operation.Extend(lengthAst) =>
        val length = evalLength(lengthAst)
        runtime.stack.extend(length)
        updateConsts(_.markConsts(FrameOffset.nonNegative(runtime.stack.frameSize() - length), Array.fill(length)(0)))
        None
      case Operation.Shrink(lengthAst) =>
        runtime.stack.shrink(evalLength(lengthAst))
        None
      case Operation.Call(offsetAst, targetAst) =>
        val newOffset = evalOffset(offsetAst)
        val target = evalSymbol(targetAst)
        runtime.stack.moveFrame(newOffset.get)
        val callerConsts = updateConsts(_.call(newOffset))
        run(target.name, depth + 1)
        updateConsts { callee => callerConsts.returnFromCall(newOffset, callee) }
        runtime.stack.moveFrame(-newOffset.get)
        None
      case Operation.CheckSize(lengthAst) =>
        assert(evalLength(lengthAst) == runtime.stack.frameSize())
        None
      case Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
        val length = evalLength(lengthAst)
        val modifier = evalSymbol(modifierAst)
        val op1 = constOrAddressRef(op1Ast, length)
        val op2 = constOrAddressRef(op2Ast, length)
        val flag = modifier.name match {
          case "eq" => RuntimeOps.equals(length, op1, op2)
          case "neq" => !RuntimeOps.equals(length, op1, op2)
          case "le" => RuntimeOps.less(length, op1, op2, orEqual = true)
          case "m" => !RuntimeOps.less(length, op1, op2, orEqual = true)
          case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifier''")
        }
        if (flag) Some(evalSymbol(targetAst)) else None
      case Operation.Jump(targetAst) =>
        Some(evalSymbol(targetAst))
      case Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
        val length = evalLength(lengthAst)
        RuntimeOps.add(length, constOrAddressRef(op1Ast, length), constOrAddressRef(op2Ast, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(op1Ast, length) && checkConst(op2Ast, length))
        None
      case Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
        val length = evalLength(lengthAst)
        RuntimeOps.mult(length, constOrAddressRef(op1Ast, length), constOrAddressRef(op2Ast, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(op1Ast, length) && checkConst(op2Ast, length))
        None
      case Operation.Neg(lengthAst, opAst, dstAst) =>
        val length = evalLength(lengthAst)
        RuntimeOps.neg(length, constOrAddressRef(opAst, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(opAst, length))
        None
      case Operation.NotSpecialize(lengthAst, dstAst) =>
        val length = evalLength(lengthAst)
        markConst(dstAst, length, isConst = false)
        None
      case Operation.Set(lengthAst, srcAst, dstAst) =>
        val length = evalLength(lengthAst)
        RuntimeOps.set(length, constOrAddressRef(srcAst, length), constOrAddressRef(dstAst, length))
        markConst(dstAst, length, isConst = checkConst(srcAst, length))
        None
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  private def constOrAddressRef(constOrAddress: Const | Address, constExpectedLength: Int): MemoryRef =
    constOrAddress match {
      case const: Const =>
        new MemoryRef(Memory.ofBytes(evalConst(const).expand(constExpectedLength).get(), ro = true), 0)
      case address: Address =>
        addressRef(address)
    }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.Stack(offset) =>
      runtime.stack.getRef(evalOffset(offset))
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const related logic
  private def markConst(dst: Address, length: Int, isConst: Boolean): Unit = dst match {
    case Address.Stack(offsetAst) =>
      val offset = evalOffset(offsetAst)
      if (isConst)
        updateConsts(_.markConsts(offset, runtime.stack.getRef(offset).get(length)))
      else
        updateConsts(_.unmarkConsts(offset, length))
    case Address.Native(_) =>
      // do nothing
    case Address.StackOffset(_, _, _) =>
      ???
  }

  override def frameSize(): Int = runtime.stack.frameSize()

  override def symbols(): Symbols = runtime.symbols

  override def get(from: FrameOffset, length: Int): Array[Byte] = runtime.stack.getRef(from).get(length)

  override def isConst(from: FrameOffset, length: Int): Boolean =
    !checkConsts || consts.isConst(from, length)

  private def updateConsts(fn: Consts => Consts): Consts =
    if (checkConsts) {
      val prev = consts
      consts = fn(consts)
      prev
    } else consts
}
