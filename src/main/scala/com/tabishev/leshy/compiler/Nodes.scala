package com.tabishev.leshy.compiler

import com.tabishev.leshy.node.Node
import com.tabishev.leshy.runtime.Runtime
import com.tabishev.leshy.runtime.FrameOffset

object Nodes {
  final case class Origin(op: OperationRef, ctx: SpecializationContext)

  final case class Link(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node.Indirect {
    private var resolved: Node = null

    override def tryResolve(): Option[Node] = Option(resolved)

    override def resolve(): Node = {
      if (resolved == null) resolved = compiler.create(op, ctx)
      resolved
    }
  }

  final case class Execute(compiler: Compiler, origin: Origin, next: Node, execution: Execution) extends Node.Run {
    override def replace(withNext: Node): Node.Run = copy(next = withNext)

    override def execute(runtime: Runtime): Unit = execution.execute(runtime)
  }

  def execute(compiler: Compiler, origin: Origin, execution: Execution): Execute = {
    val (stackSize, prevConsts) = origin.ctx.get()
    val nextCtx = SpecializationContext.from(execution.stackSize(stackSize), execution.markConsts(prevConsts))
    Execute(compiler, origin, Link(compiler, nextCtx, origin.op.next), execution)
  }

  final case class Branch(compiler: Compiler, origin: Origin, ifTrue: Node, ifFalse: Node, execution: BranchExecution) extends Node.Branch {
    override def replace(withIfTrue: Node, withIfFalse: Node): Node.Branch =
      copy(ifTrue = withIfTrue, ifFalse = withIfFalse)

    override def execute(runtime: Runtime): Boolean = execution.execute(runtime)
  }

  def branch(compiler: Compiler, origin: Origin, execution: BranchExecution, target: OperationRef): Branch =
    Branch(compiler, origin, Link(compiler, origin.ctx, target), Link(compiler, origin.ctx, origin.op.next), execution)

  final case class Call(compiler: Compiler, origin: Origin, call: Node, offset: FrameOffset) extends Node.Call {
    private var next: Map[Node.Final, Node] = Map()

    override def replace(withCall: Node): Node.Call = copy(call = withCall)

    override def next(returnNode: Node.Final): Node =
      next.getOrElse(returnNode, {
        val calleeCtx = returnNode.asInstanceOf[Final].origin.ctx
        // depending on calculation/specializations being made by callee next line node might be different
        val nextCtx = SpecializationContext.fnCall(origin.ctx, offset, calleeCtx)
        val node = compiler.create(origin.op.next, nextCtx)
        next = next.updated(returnNode, node)
        node
      })
  }

  def call(compiler: Compiler, origin: Origin, offset: FrameOffset, target: String): Call = {
    val node = Link(compiler, SpecializationContext.offset(origin.ctx, offset), OperationRef(target, 0))
    Call(compiler, origin, node, offset)
  }

  final case class Final(compiler: Compiler, origin: Origin) extends Node.Final
}
