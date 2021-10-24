package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Bytes, Fn, OperationWithSource}
import com.tabishev.leshy.interpreter.ConstInterpreter

import scala.util.Try

object NodeFactory {
  def create(compiler: Compiler, constInterpreter: ConstInterpreter,
             ctx: SpecializationContext, op: OperationRef, fn: Fn): Node = {
    def simpleNode(impl: SimpleImpl): Node = Node.Simple(compiler, ctx, op, impl)
    def toOperand(address: ast.Address): MemoryOperand = toOperandFn(constInterpreter, address)
    def toIntOrOperand(addressOrConst: ast.Const | ast.Address): Int | MemoryOperand = toIntOrOperandFn(constInterpreter, addressOrConst)
    def toLongOrOperand(addressOrConst: ast.Const | ast.Address): Long | MemoryOperand = toLongOrOperandFn(constInterpreter, addressOrConst)

    op.resolve(fn) match {
      case None => Node.Final(compiler, ctx, op)
      case Some(operation) => operation.op match {
        case ast.Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.SetStackSize(compiler, ctx, op, constInterpreter.frameSize() + length)
        case ast.Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          Node.SetStackSize(compiler, ctx, op, constInterpreter.frameSize() - length)
        case ast.Operation.Append(bytesAst) => ???
        case ast.Operation.Call(offsetAst, targetAst) =>
          val offsetRaw = constInterpreter.evalConst(offsetAst).asExpandedInt.get
          val offset = if (offsetRaw >= 0) offsetRaw else constInterpreter.frameSize() + offsetRaw
          val target = constInterpreter.evalSymbol(targetAst).name
          Node.Call(compiler, ctx, op, offset, target)
        case ast.Operation.CheckSize(lengthAst) =>
          Node.Throw(compiler, ctx, op, Try {
            assert(constInterpreter.evalConst(lengthAst).asExpandedInt.get == constInterpreter.frameSize())
          })
        case ast.Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)

          val impl = (length, modifier) match {
            case (4, "m") => Branch.more4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast))
            case (4, "le") => Branch.lessOrEqual4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast))

            case (8, "m") => Branch.more8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast))
            case (8, "le") => Branch.lessOrEqual8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast))

            case _ =>
              throw new UnsupportedOperationException(length + " " + modifier)
          }

          Node.Branch(compiler, ctx, op, impl, target)
        case ast.Operation.Jump(targetAst) =>
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)
          Node.Branch(compiler, ctx, op, Branch.Always, target)
        case ast.Operation.PrintInt(length, src) => ???
        case ast.Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Sum.length4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast), dst)
            case 8 => Sum.length8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast), dst)
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Mult.length4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast), dst)
            case 8 => Mult.length8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast), dst)
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Negate.length4(toIntOrOperand(opAst), dst)
            case 8 => Negate.length8(toLongOrOperand(opAst), dst)
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Set.length4(toIntOrOperand(srcAst), dst)
            case 8 => Set.length8(toLongOrOperand(srcAst), dst)
            case _ => ???
          }
          simpleNode(impl)
        case ast.Operation.NonConst(lengthAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asExpandedInt.get
          simpleNode(NonConst.Mark(length, toOperand(dstAst)))
        case op =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }
  }

  private def label(fn: Fn, ctx: OperationRef, label: String): OperationRef =
    OperationRef(ctx.fn, fn.labels(label))

  private def toOperandFn(constInterpreter: ConstInterpreter, address: ast.Address): MemoryOperand = address match {
    case ast.Address.Stack(offsetAst) =>
      val rawOffset = constInterpreter.evalConst(offsetAst).asExpandedInt.get
      val offset = if (rawOffset < 0) constInterpreter.frameSize() + rawOffset else rawOffset
      MemoryOperand.Stack(offset)
    case ast.Address.StackOffset(_, _, _) =>
      ???
    case ast.Address.Native(_) =>
      ???
  }

  private def toIntOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: ast.Address | ast.Const): Int | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 4, _.asInt.get)

  private def toLongOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: ast.Address | ast.Const): Long | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 8, _.asLong.get)

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
