package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.lang.ast.{Address, Const, Fn, Operation}
import com.tabishev.leshy.lang.common.{ConstInterpreter, Symbols}
import com.tabishev.leshy.lang.loader.FnLoader
import com.tabishev.leshy.node._
import com.tabishev.leshy.runtime.{Bytes, FrameOffset}

final case class Origin(loader: FnLoader, symbols: Symbols, op: OperationRef, ctx: SpecializationContext) {
  override def toString: String = s"$op, $ctx"
}

trait LeshyNode {
  val origin: Origin
}

object Nodes {
  def create(origin: Origin): Node = {
    // todo: do we really need it?
    val fn = origin.loader.load(origin.op.fn).get
    // todo: make it not running every time
    origin.symbols.register(fn)

    val constInterpreter = SpecializationContextConstInterpreter(origin.symbols, origin.ctx)

    def toOperand(address: Address): MemoryOperand = toOperandFn(constInterpreter, address)
    def arg(length: Int, addressOrConst: Either[Const, Address]): Either[Bytes, MemoryOperand] = toBytesOrOperandFn(constInterpreter, addressOrConst, length)

    origin.op.resolve(fn) match {
      case None => Final(origin)
      case Some(operation) => operation.op match {
        case Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          SetSize(origin, constInterpreter.frameSize() + length)
        case Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          SetSize(origin, constInterpreter.frameSize() - length)
        case Operation.Call(offsetAst, targetAst) =>
          val offset = constInterpreter.evalOffset(offsetAst)
          val target = constInterpreter.evalSymbol(targetAst).name
          Call(origin, offset, target)
        case Operation.CheckSize(lengthAst) =>
          assert(constInterpreter.evalLength(lengthAst) == constInterpreter.frameSize())
          Jump(origin, origin.op.next)
        case Operation.Branch(modifierAst, lengthAst, op1Ast, op2Ast, targetAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val modifier = constInterpreter.evalSymbol(modifierAst).name
          val target = label(fn, origin.op, constInterpreter.evalSymbol(targetAst).name)

          val conditionModifier = modifier match {
            case "gt" => ConditionModifier.GT
            case "le" => ConditionModifier.LE
            case "eq" => ConditionModifier.EQ
            case _ => throw new UnsupportedOperationException(modifier)
          }
          val impl = Condition.Binary(length, arg(length, op1Ast), conditionModifier, arg(length, op2Ast))
          Branch(origin, impl, target)
        case Operation.Jump(targetAst) =>
          val target = label(fn, origin.op, constInterpreter.evalSymbol(targetAst).name)
          Jump(origin, target)
        case Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          CommandRun(origin, Command.Sum(length, dst, arg(length, op1Ast), arg(length, op2Ast)))
        case Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          CommandRun(origin, Command.Mult(length, dst, arg(length, op1Ast), arg(length, op2Ast)))
        case Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          CommandRun(origin, Command.Negate(length, dst, arg(length, opAst)))
        case Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          CommandRun(origin, Command.Set(length, dst, arg(length, srcAst)))
        case Operation.NotSpecialize(lengthAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          NotSpecialize(origin, toOperand(dstAst), length)
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

  abstract class Execute extends Node.Run with LeshyNode {
    def specialize(before: SpecializationContext): SpecializationContext

    final override def next: Node = Nodes.create(origin.copy(op = origin.op.next, ctx = specialize(origin.ctx)))
  }

  final case class CommandRun(origin: Origin, command: Command) extends Execute {
    override def specialize(before: SpecializationContext): SpecializationContext = before.afterCommand(command)
  }

  // Specialize can't implemented similarly because execution assumes spec ctx not changing between runs
  final case class NotSpecialize(origin: Origin, dst: MemoryOperand, length: Int) extends Execute {
    override def command: Command = Command.Noop

    override def specialize(before: SpecializationContext): SpecializationContext = before.notSpecialize(dst, length)
  }

  final case class SetSize(origin: Origin, size: Int) extends Execute {
    override def command: Command = Command.SetFramesize(size)

    override def specialize(before: SpecializationContext): SpecializationContext = before.setSize(size)
  }

  final case class Jump(origin: Origin, nextOp: OperationRef) extends Node.Run with LeshyNode {
    override def command: Command = Command.Noop

    override def next: Node = Nodes.create(origin.copy(op = nextOp))
  }

  final case class Branch(origin: Origin, condition: Condition, target: OperationRef) extends Node.Branch with LeshyNode {
    override def ifTrue: Node = Nodes.create(origin.copy(op = target))

    override def ifFalse: Node = Nodes.create(origin.copy(op = origin.op.next))
  }

  final case class Call(origin: Origin, offset: FrameOffset, target: String) extends Node.Call with LeshyNode {
    override def call: Node = Nodes.create(origin.copy(op = OperationRef(target, 0), ctx = origin.ctx.offset(offset)))

    override def next(returnNode: Node.Final): Node = {
      val calleeCtx = returnNode.asInstanceOf[Final].origin.ctx
      // depending on calculation/specializations being made by callee next line node might be different
      val nextCtx = origin.ctx.fnCall(offset, calleeCtx)
      Nodes.create(origin.copy(op = origin.op.next, ctx = nextCtx))
    }
  }

  final case class Final(origin: Origin) extends Node.Final with LeshyNode
}
