package com.tabishev.leshy.lang.compiler

import com.tabishev.leshy.runtime.Bytes
import com.tabishev.leshy.lang.ast.{Address, Const, Fn, Operation}
import com.tabishev.leshy.lang.common.{ConstInterpreter, Symbols}
import com.tabishev.leshy.lang.loader.FnLoader
import com.tabishev.leshy.node.{Command, Condition, MemoryOperand, Node}
import com.tabishev.leshy.runtime.FrameOffset

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
    def arg(length: Int, addressOrConst: Const | Address): Bytes | MemoryOperand = toBytesOrOperandFn(constInterpreter, addressOrConst, length, identity)

    origin.op.resolve(fn) match {
      case None => Final(origin)
      case Some(operation) => operation.op match {
        case Operation.Extend(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          Execute(origin, Executions.SetSize(constInterpreter.frameSize() + length))
        case Operation.Shrink(lengthAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          Execute(origin, Executions.SetSize(constInterpreter.frameSize() - length))
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

          val impl = (length, modifier) match {
            case (length, "m") => Condition.Gt(length, arg(length, op1Ast), arg(length, op2Ast))
            case (length, "le") => Condition.Le(length, arg(length, op1Ast), arg(length, op2Ast))
            case (length, "eq") => Condition.Eq(length, arg(length, op1Ast), arg(length, op2Ast))

            case _ =>
              throw new UnsupportedOperationException(length + " " + modifier)
          }

          Branch(origin, impl, target)
        case Operation.Jump(targetAst) =>
          val target = label(fn, origin.op, constInterpreter.evalSymbol(targetAst).name)
          Jump(origin, target)
        case Operation.Add(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          Execute(origin, Executions.Simple(Command.Sum(length, dst, arg(length, op1Ast), arg(length, op2Ast))))
        case Operation.Mult(lengthAst, op1Ast, op2Ast, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          Execute(origin, Executions.Simple(Command.Mult(length, dst, arg(length, op1Ast), arg(length, op2Ast))))
        case Operation.Neg(lengthAst, opAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          Execute(origin, Executions.Simple(Command.Negate(length, dst, arg(length, opAst))))
        case Operation.Set(lengthAst, srcAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          val dst = toOperand(dstAst)
          Execute(origin, Executions.Simple(Command.Set(length, dst, arg(length, srcAst))))
        case Operation.NotSpecialize(lengthAst, dstAst) =>
          val length = constInterpreter.evalLength(lengthAst)
          Execute(origin, Executions.NotSpecialize(toOperand(dstAst), length))
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

  private def toIntOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: Address | Const): Int | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 4, _.asInt)

  private def toLongOrOperandFn(constInterpreter: ConstInterpreter, addressOrConst: Address | Const): Long | MemoryOperand =
    toBytesOrOperandFn(constInterpreter, addressOrConst, 8, _.asLong)

  private def toBytesOrOperandFn[T](constInterpreter: ConstInterpreter, addressOrConst: Address | Const, length: Int, transform: Bytes => T): T | MemoryOperand =
    addressOrConst match {
      case const: Const =>
        transform(constInterpreter.evalConst(const).expand(length))
      case address: Address =>
        constInterpreter.tryConst(address, length) match {
          case Some(bytes) => transform(bytes)
          case None => toOperandFn(constInterpreter, address)
        }
    }

  final case class Execute(origin: Origin, execution: Execution) extends Node.Run with LeshyNode {
    override def command: Command = execution.command

    override def next: Node = Nodes.create(origin.copy(op = origin.op.next, ctx = execution.specialize(origin.ctx)))
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
