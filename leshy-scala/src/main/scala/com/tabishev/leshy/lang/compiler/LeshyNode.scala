package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.lang.ast.{Address, Const, Fn, Operation}
import com.tabishev.leshy.lang.common.{ConstInterpreter, Symbols}
import com.tabishev.leshy.lang.loader.FnLoader
import com.tabishev.leshy.node._
import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

case class LeshyNode(loader: FnLoader, symbols: Symbols, op: OperationRef, ctx: SpecializationContext) extends Node {
  override def get(): Node.Kind = {
    // todo: do we really need it?
    val fn = loader.load(op.fn).get
    // todo: make it not running every time
    symbols.register(fn)

    val constInterpreter = SpecializationContextConstInterpreter(symbols, ctx)

    def toOperand(address: Address): MemoryOperand = toOperandFn(constInterpreter, address)
    def arg(length: Int, addressOrConst: Either[Const, Address]): Either[Bytes, MemoryOperand] = toBytesOrOperandFn(constInterpreter, addressOrConst, length)

    op.resolve(fn) match {
      case None => Node.Final
      case Some(operation) => operation.op match {
        case Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          setSize(constInterpreter.frameSize() + length)
        case Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          setSize(constInterpreter.frameSize() - length)
        case Operation.Call(offsetAst, targetAst) =>
          val offset = constInterpreter.evalOffset(offsetAst)
          val target = constInterpreter.evalSymbol(targetAst).name
          call(offset, target)
        case Operation.CheckSize(lengthAst) =>
          assert(constInterpreter.evalLength(lengthAst) == constInterpreter.frameSize())
          jump(op.next)
        case Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)

          val conditionModifier = modifier match {
            case "gt" => ConditionModifier.GT
            case "le" => ConditionModifier.LE
            case "eq" => ConditionModifier.EQ
            case _ => throw new UnsupportedOperationException(modifier)
          }
          val impl = Condition.Binary(length, arg(length, op1Ast), conditionModifier, arg(length, op2Ast))
          branch(impl, target)
        case Operation.Jump(targetAst) =>
          val target = label(fn, op, constInterpreter.evalSymbol(targetAst).name)
          jump(target)
        case Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          commandRun(Command.Sum(length, dst, arg(length, op1Ast), arg(length, op2Ast)))
        case Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          commandRun(Command.Mult(length, dst, arg(length, op1Ast), arg(length, op2Ast)))
        case Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          commandRun(Command.Negate(length, dst, arg(length, opAst)))
        case Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          commandRun(Command.Set(length, dst, arg(length, srcAst)))
        case Operation.NotSpecialize(lengthAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          notSpecialize(toOperand(dstAst), length)
        case op =>
          throw new IllegalArgumentException(s"unsupported operation '$op''")
      }
    }
  }

  private def label(fn: Fn, ctx: OperationRef, label: String): OperationRef =
    OperationRef(ctx.fn, fn.labels(label))

  private def toOperandFn(constInterpreter: ConstInterpreter, address: Address): MemoryOperand = address match {
    case Address.Stack(offsetAst) =>
      MemoryOperand.Stack(constInterpreter.evalOffset(offsetAst))
    case Address.StackOffset(_, _, _) =>
      ???
    case Address.Native(_) =>
      ???
  }

  private def toBytesOrOperandFn[T](constInterpreter: ConstInterpreter, addressOrConst: Either[Const, Address], length: Int): Either[Bytes, MemoryOperand] =
    addressOrConst match {
      case Left(const) =>
        Left(constInterpreter.evalConst(const).expand(length))
      case Right(address) =>
        constInterpreter.tryConst(Right(address), length) match {
          case Some(bytes) => Left(bytes)
          case None => Right(toOperandFn(constInterpreter, address))
        }
    }

  private def execute(command: Command, specialize: SpecializationContext => SpecializationContext): Node.Run =
    Node.Run(command, copy(op = op.next, ctx = specialize(ctx)))

  private def commandRun(command: Command): Node.Run = execute(command, _.afterCommand(command))

  // Specialize can't implemented similarly because execution assumes spec ctx not changing between runs
  private def notSpecialize(dst: MemoryOperand, length: Int): Node.Run =
    execute(Command.Noop, _.notSpecialize(dst, length))

  private def setSize(size: Int): Node.Run =
    execute(Command.SetFramesize(size), _.setSize(size))

  private def jump(nextOp: OperationRef): Node.Run =
    Node.Run(Command.Noop, copy(op = nextOp))

  private def branch(condition: Condition, target: OperationRef): Node.Branch =
    Node.Branch(condition, copy(op = target), copy(op = op.next))

  private def call(offset: FrameOffset, target: String): Node.Call = {
    val callNode = copy(op = OperationRef(target, 0), ctx = ctx.offset(offset))
    val nextNode = (returnNode: Node) => {
      assert(returnNode.get() == Node.Final)
      val calleeCtx = returnNode.asInstanceOf[LeshyNode].ctx
      // depending on calculation/specializations being made by callee next line node might be different
      val nextCtx = ctx.fnCall(offset, calleeCtx)
      copy(op = op.next, ctx = nextCtx)
    }
    Node.Call(offset, callNode, nextNode)
  }

  override def toString: String = s"$op, $ctx"
}
