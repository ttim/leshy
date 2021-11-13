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

  final case class Execute(compiler: Compiler, origin: Origin, execution: Execution) extends Node.Run {
    override def execute(runtime: Runtime): Unit = execution.execute(runtime)

    override val next: Node = {
      val (stackSize, prevConsts) = origin.ctx.get()
      val nextCtx = SpecializationContext.from(execution.stackSize(stackSize), execution.markConsts(prevConsts))
      Link(compiler, nextCtx, origin.op.next)
    }
  }

  final case class Branch(compiler: Compiler, origin: Origin, execution: BranchExecution, target: OperationRef) extends Node.Branch {
    override val ifTrue: Node = Link(compiler, origin.ctx, target)
    override val ifFalse: Node = Link(compiler, origin.ctx, origin.op.next)

    override def execute(runtime: Runtime): Boolean = execution.execute(runtime)
  }

  final case class Call(compiler: Compiler, origin: Origin, offset: FrameOffset, target: String) extends Node.Call {
    private var next: Map[Node.Final, Node] = Map()

    override val call: Node =
      Link(compiler, SpecializationContext.offset(origin.ctx, offset), OperationRef(target, 0))

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

  final case class Final(compiler: Compiler, origin: Origin) extends Node.Final
}
