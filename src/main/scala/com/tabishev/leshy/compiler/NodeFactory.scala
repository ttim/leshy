package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast
import com.tabishev.leshy.ast.{Bytes, Fn}
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.runtime.FrameOffset

import scala.annotation.tailrec
import scala.util.Try

object NodeFactory {
  @tailrec
  def create(compiler: Compiler, constInterpreter: ConstInterpreter,
             ctx: SpecializationContext, op: OperationRef, fn: Fn): Node = {
    val options = Node.Options(compiler.debugEnabled, maintainContext = true, op, ctx)

    def node(op: OperationRef): GenericNode = GenericNode.of(ctx => compiler.create(op, ctx))

    def executeNode(execution: Execution): Node = Node.Run(options, execution, node(op.next))
    def toOperand(address: ast.Address): MemoryOperand = toOperandFn(constInterpreter, address)
    def toIntOrOperand(addressOrConst: ast.Const | ast.Address): Int | MemoryOperand = toIntOrOperandFn(constInterpreter, addressOrConst)
    def toLongOrOperand(addressOrConst: ast.Const | ast.Address): Long | MemoryOperand = toLongOrOperandFn(constInterpreter, addressOrConst)

    op.resolve(fn) match {
      case None => Node.Final(options)
      case Some(operation) => operation.op match {
        case ast.Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          executeNode(Stack.SetSize(constInterpreter.frameSize(), constInterpreter.frameSize() + length))
        case ast.Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          executeNode(Stack.SetSize(constInterpreter.frameSize(), constInterpreter.frameSize() - length))
        case ast.Operation.Call(offsetAst, targetAst) =>
          val offsetRaw = constInterpreter.evalConst(offsetAst).asInt
          val offset = if (offsetRaw >= 0) offsetRaw else constInterpreter.frameSize() + offsetRaw
          val target = constInterpreter.evalSymbol(targetAst).name
          Node.Call(options, offset, node(OperationRef(target, 0)), node(op.next))
        case ast.Operation.CheckSize(lengthAst) =>
          assert(constInterpreter.evalConst(lengthAst).asInt == constInterpreter.frameSize())
          create(compiler, constInterpreter, ctx, op.next, fn)
        case ast.Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)

          val impl = (length, modifier) match {
            case (4, "m") => BranchExecution.more4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast))
            case (4, "le") => BranchExecution.lessOrEqual4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast))

            case (8, "m") => BranchExecution.more8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast))
            case (8, "le") => BranchExecution.lessOrEqual8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast))

            case _ =>
              throw new UnsupportedOperationException(length + " " + modifier)
          }

          Node.Branch(options, impl, node(target), node(op.next))
        case ast.Operation.Jump(targetAst) =>
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)
          create(compiler, constInterpreter, ctx, target, fn)
        case ast.Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Sum.length4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast), dst)
            case 8 => Sum.length8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast), dst)
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Mult.length4(toIntOrOperand(op1Ast), toIntOrOperand(op2Ast), dst)
            case 8 => Mult.length8(toLongOrOperand(op1Ast), toLongOrOperand(op2Ast), dst)
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Negate.length4(toIntOrOperand(opAst), dst)
            case 8 => Negate.length8(toLongOrOperand(opAst), dst)
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          val dst = toOperand(dstAst)
          val impl = length match {
            case 4 => Set.length4(toIntOrOperand(srcAst), dst)
            case 8 => Set.length8(toLongOrOperand(srcAst), dst)
            case _ => ???
          }
          executeNode(impl)
        case ast.Operation.NotSpecialize(lengthAst, dstAst) =>
          val length = constInterpreter.evalConst(lengthAst).asInt
          executeNode(Mark.NotSpecialize(length, toOperand(dstAst)))
        case op =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }
  }

  private def label(fn: Fn, ctx: OperationRef, label: String): OperationRef =
    OperationRef(ctx.fn, fn.labels(label))

  private def toOperandFn(constInterpreter: ConstInterpreter, address: ast.Address): MemoryOperand = address match {
    case ast.Address.Stack(offsetAst) =>
      MemoryOperand.Stack(FrameOffset.maybeNegative(constInterpreter.evalConst(offsetAst).asInt, constInterpreter.frameSize()))
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
