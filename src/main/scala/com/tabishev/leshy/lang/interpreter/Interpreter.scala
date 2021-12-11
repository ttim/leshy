package com.tabishev.leshy.lang.interpreter

import com.tabishev.leshy.lang.ast.{Address, Const, Operation, OperationWithSource}
import com.tabishev.leshy.lang.common.{ConstInterpreter, Consts, FnSpec, Symbol, Symbols}
import com.tabishev.leshy.lang.loader.FnLoader
import com.tabishev.leshy.runtime.*

import java.io.File
import java.nio.ByteBuffer
import java.util
import scala.collection.mutable

class Interpreter(loader: FnLoader, debug: Boolean, checkConsts: Boolean) extends ConstInterpreter {
  private val stack = new StackMemory()
  private val interpreterSymbols = new Symbols()
  private var consts = Consts.Empty

  def run[T, V](spec: FnSpec[T, V])(input: T): V = {
    val inputObj = spec.input(input)

    assert(stack.isEmpty())
    stack.append(inputObj.bytes)
    updateConsts(_ => inputObj.consts)
    try {
      run(spec.fn, 0)
      assert(stack.getFrameOffset() == 0)
      spec.output(Bytes.fromBytes(stack.currentStackFrame()))
    } finally {
      stack.clean()
      updateConsts(_ => Consts.Empty)
    }
  }

  // for rest
  private def run(name: String, depth: Int): Unit = {
    val fn = loader.load(name).get
    interpreterSymbols.register(fn)

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
    if (debug) println("\t".repeat(depth) + s"${consts.frameToString(stack.currentStackFrame())}: ${op.op}")
    op.op match {
      case Operation.Extend(lengthAst) =>
        val length = evalLength(lengthAst)
        stack.extend(length)
        updateConsts(_.markConsts(FrameOffset.nonNegative(stack.frameSize() - length), Array.fill(length)(0)))
        None
      case Operation.Shrink(lengthAst) =>
        stack.shrink(evalLength(lengthAst))
        None
      case Operation.Call(offsetAst, targetAst) =>
        val newOffset = evalOffset(offsetAst)
        val target = evalSymbol(targetAst)
        stack.moveFrame(newOffset.get)
        val callerConsts = updateConsts(_.call(newOffset))
        run(target.name, depth + 1)
        updateConsts { callee => callerConsts.returnFromCall(newOffset, callee) }
        stack.moveFrame(-newOffset.get)
        None
      case Operation.CheckSize(lengthAst) =>
        assert(evalLength(lengthAst) == stack.frameSize())
        None
      case Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
        val length = evalLength(lengthAst)
        val modifier = evalSymbol(modifierAst)
        val op1 = constOrAddressRef(op1Ast, length)
        val op2 = constOrAddressRef(op2Ast, length)
        val flag = modifier.name match {
          case "eq" => Ops.equals(length, op1, op2)
          case "ne" => !Ops.equals(length, op1, op2)
          case "le" => Ops.less(length, op1, op2, orEqual = true)
          case "gt" => !Ops.less(length, op1, op2, orEqual = true)
          case _ => throw new IllegalArgumentException(s"unsupported branch modifier '$modifier''")
        }
        if (flag) Some(evalSymbol(targetAst)) else None
      case Operation.Jump(targetAst) =>
        Some(evalSymbol(targetAst))
      case Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
        val length = evalLength(lengthAst)
        Ops.add(length, constOrAddressRef(op1Ast, length), constOrAddressRef(op2Ast, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(op1Ast, length) && checkConst(op2Ast, length))
        None
      case Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
        val length = evalLength(lengthAst)
        Ops.mult(length, constOrAddressRef(op1Ast, length), constOrAddressRef(op2Ast, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(op1Ast, length) && checkConst(op2Ast, length))
        None
      case Operation.Neg(lengthAst, opAst, dstAst) =>
        val length = evalLength(lengthAst)
        Ops.neg(length, constOrAddressRef(opAst, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(opAst, length))
        None
      case Operation.NotSpecialize(lengthAst, dstAst) =>
        val length = evalLength(lengthAst)
        markConst(dstAst, length, isConst = false)
        None
      case Operation.Set(lengthAst, srcAst, dstAst) =>
        val length = evalLength(lengthAst)
        Ops.set(length, constOrAddressRef(srcAst, length), addressRef(dstAst))
        markConst(dstAst, length, isConst = checkConst(srcAst, length))
        None
      case _ =>
        throw new IllegalArgumentException(s"unsupported operation '$op''")
    }
  }

  private def constOrAddressRef(constOrAddress: Either[Const, Address], constExpectedLength: Int): MemoryRef =
    constOrAddress match {
      case Left(const) =>
        new MemoryRef(Memory.ofBytes(evalConst(const).expand(constExpectedLength).get(), ro = true), 0)
      case Right(address) =>
        addressRef(address)
    }

  private def addressRef(address: Address): MemoryRef = address match {
    case Address.Stack(offset) =>
      stack.getRef(evalOffset(offset))
    case _ =>
      throw new UnsupportedOperationException(s"unsupported address: $address")
  }

  // const related logic
  private def markConst(dst: Address, length: Int, isConst: Boolean): Unit = dst match {
    case Address.Stack(offsetAst) =>
      val offset = evalOffset(offsetAst)
      if (isConst)
        updateConsts(_.markConsts(offset, stack.getRef(offset).get(length)))
      else
        updateConsts(_.unmarkConsts(offset, length))
    case Address.Native(_) =>
      // do nothing
    case Address.StackOffset(_, _, _) =>
      ???
  }

  override def frameSize(): Int = stack.frameSize()

  override def symbols(): Symbols = interpreterSymbols

  override def get(from: FrameOffset, length: Int): Array[Byte] = stack.getRef(from).get(length)

  override def isConst(from: FrameOffset, length: Int): Boolean =
    !checkConsts || consts.isConst(from, length)

  private def updateConsts(fn: Consts => Consts): Consts =
    if (checkConsts) {
      val prev = consts
      consts = fn(consts)
      prev
    } else consts
}
