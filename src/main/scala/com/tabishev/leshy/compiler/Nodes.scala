package com.tabishev.leshy.compiler

import com.tabishev.leshy.node.Node
import com.tabishev.leshy.runtime.Runtime
import com.tabishev.leshy.runtime.FrameOffset

object Nodes {
  final case class Origin(op: OperationRef, ctx: SpecializationContext)

  final case class Link(compiler: Compiler, ctx: SpecializationContext, op: OperationRef) extends Node.Indirect {
    override def resolve(): Node = compiler.create(op, ctx)
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
    override val call: Node = Link(compiler, SpecializationContext.offset(origin.ctx, offset), OperationRef(target, 0))

    // todo: add caching!
    //    var next: Map[Node.Final, Node] = Map()
    //
    //    private def nextNode(calleeFinalNode: Node.Final): Node =
    //      next.getOrElse(calleeFinalNode, {
    //        val node = supplyNext(calleeFinalNode)
    //        next = next.updated(calleeFinalNode, node)
    //        node
    //      })

    override def next(returnNode: Node.Final): Node = {
      val calleeCtx = returnNode.asInstanceOf[Final].origin.ctx
      // depending on calculation/specializations being made by callee next line node might be different
      val nextCtx = SpecializationContext.fnCall(origin.ctx, offset, calleeCtx)
      compiler.create(origin.op.next, nextCtx)
    }
  }

  final case class Final(compiler: Compiler, origin: Origin) extends Node.Final
}
