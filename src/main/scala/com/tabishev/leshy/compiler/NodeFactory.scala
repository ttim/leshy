package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Bytes, Fn}
import com.tabishev.leshy.runtime.Runtime
import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.compiler.Nodes.Origin
import com.tabishev.leshy.node.Node
import com.tabishev.leshy.runtime.{FrameOffset, Symbols}

import scala.annotation.tailrec
import scala.util.Try

object NodeFactory {
  @tailrec
  def create(compiler: Compiler, symbols: Symbols,
             ctx: SpecializationContext, op: OperationRef, fn: Fn): Node = {
    val origin = Origin(compiler, op, ctx)
    val constInterpreter = SpecializationContextConstInterpreter(symbols, ctx)

    def executeNode(execution: Execution): Node = Nodes.execute(origin, execution)
    def toOperand(address: ast.Address): MemoryOperand = toOperandFn(constInterpreter, address)
    def toIntProvider(addressOrConst: ast.Const | ast.Address): IntProvider = IntProvider.create(toIntOrOperandFn(constInterpreter, addressOrConst))
    def toLongProvider(addressOrConst: ast.Const | ast.Address): LongProvider = LongProvider.create(toLongOrOperandFn(constInterpreter, addressOrConst))

    op.resolve(fn) match {
      case None => Nodes.Final(origin)
      case Some(operation) => operation.op match {
        case ast.Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          executeNode(Stack.SetSize(constInterpreter.frameSize() + length))
        case ast.Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          executeNode(Stack.SetSize(constInterpreter.frameSize() - length))
        case ast.Operation.Call(offsetAst, targetAst) =>
          val offset = constInterpreter.evalOffset(offsetAst)
          val target = constInterpreter.evalSymbol(targetAst).name
          Nodes.call(origin, offset, target)
        case ast.Operation.CheckSize(lengthAst) =>
          assert(constInterpreter.evalLength(lengthAst) == constInterpreter.frameSize())
          create(compiler, symbols, ctx, op.next, fn)
        case ast.Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)

          val impl = (length, modifier) match {
            case (4, "m") => Branches.Gt4(toIntProvider(op1Ast), toIntProvider(op2Ast))
            case (4, "le") => Branches.Le4(toIntProvider(op1Ast), toIntProvider(op2Ast))

            case (8, "m") => Branches.Gt8(toLongProvider(op1Ast), toLongProvider(op2Ast))
            case (8, "le") => Branches.Le8(toLongProvider(op1Ast), toLongProvider(op2Ast))

            case _ =>
              throw new UnsupportedOperationException(length + " " + modifier)
          }

          Nodes.branch(origin, impl, target)
        case ast.Operation.Jump(targetAst) =>
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)
          create(compiler, symbols, ctx, target, fn)
        case ast.Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Sum.Length4(dst, toIntProvider(op1Ast), toIntProvider(op2Ast))
            case 8 => Sum.Length8(dst, toLongProvider(op1Ast), toLongProvider(op2Ast))
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Mult.Length4(dst, toIntProvider(op1Ast), toIntProvider(op2Ast))
            case 8 => Mult.Length8(dst, toLongProvider(op1Ast), toLongProvider(op2Ast))
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Negate.Length4(dst, toIntProvider(opAst))
            case 8 => Negate.Length8(dst, toLongProvider(opAst))
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Set.Length4(dst, toIntProvider(srcAst))
            case 8 => Set.Length8(dst, toLongProvider(srcAst))
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.NotSpecialize(lengthAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          executeNode(Mark.NotSpecialize(toOperand(dstAst), length))
        case op =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }
  }

  private def label(fn: Fn, ctx: OperationRef, label: String): OperationRef =
    OperationRef(ctx.fn, fn.labels(label))

  private def toOperandFn(constInterpreter: ConstInterpreter, address: ast.Address): MemoryOperand = address match {
    case ast.Address.Stack(offsetAst) =>
      MemoryOperand.Stack(constInterpreter.evalOffset(offsetAst))
    case ast.Address.StackOffset(_, _, _) =>
      ???
    case ast.Address.Native(_) =>
      ???
  }

  private def toIntOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: ast.Address | ast.Const): Int | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 4, _.asInt)

  private def toLongOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: ast.Address | ast.Const): Long | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 8, _.asLong)

  private def toBytesOrOperandFn[T](constInterpreter: ConstInterpreter, addressOrConst: ast.Address | ast.Const, length: Int, transform: Bytes => T): T | MemoryOperand =
    addressOrConst match {
      case const: ast.Const =>
        transform(constInterpreter.evalConst(const).expand(length))
      case address: ast.Address =>
        constInterpreter.tryConst(address, length) match {
          case Some(bytes) => transform(bytes)
          case None => toOperandFn(constInterpreter, address)
        }
    }
}
