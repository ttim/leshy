package com.tabishev.leshy.compiler

import com.tabishev.leshy.node.Node
import com.tabishev.leshy.runtime.Runtime
import com.tabishev.leshy.runtime.FrameOffset
import org.objectweb.asm.{Label, MethodVisitor}

object Nodes {
  final case class Origin(compiler: Compiler, op: OperationRef, ctx: SpecializationContext)

  final case class Link(origin: Origin) extends Node.Indirect {
    private var resolved: Node = null

    override def tryResolve(): Option[Node] = Option(resolved)

    override def resolve(): Node = {
      if (resolved == null) resolved = origin.compiler.create(origin.op, origin.ctx)
      resolved
    }

    def invalidate(): Unit = resolved = null
  }

  final case class Execute(origin: Origin, next: Node, execution: Execution) extends Node.Run {
    override def copy(next: Node): Node.Run = Execute(origin, next, execution)

    override def execute(runtime: Runtime): Unit = execution.execute(runtime)

    override def generate(writer: MethodVisitor): Unit = execution.write(writer)
  }

  def execute(origin: Origin, execution: Execution): Execute = {
    val (stackSize, prevConsts) = origin.ctx.get()
    val nextCtx = SpecializationContext.from(execution.stackSize(stackSize), execution.markConsts(stackSize, prevConsts))
    Execute(origin, Link(Origin(origin.compiler, origin.op.next, nextCtx)), execution)
  }

  final case class Branch(origin: Origin, ifTrue: Node, ifFalse: Node, execution: BranchExecution) extends Node.Branch {
    override def copy(ifTrue: Node, ifFalse: Node): Node.Branch = Branch(origin, ifTrue, ifFalse, execution)

    override def execute(runtime: Runtime): Boolean = execution.execute(runtime)
    override def generate(writer: MethodVisitor, ifTrue: Label): Unit = execution.generate(writer, ifTrue)
  }

  def branch(origin: Origin, execution: BranchExecution, target: OperationRef): Branch =
    Branch(origin, Link(Origin(origin.compiler, target, origin.ctx)), Link(Origin(origin.compiler, origin.op.next, origin.ctx)), execution)

  final case class Call(origin: Origin, call: Node, offset: FrameOffset) extends Node.Call {
    private var next: Map[Node.Final, Node] = Map()

    override def copy(call: Node): Node.Call = Call(origin, call, offset)

    override def next(returnNode: Node.Final): Node =
      next.getOrElse(returnNode, {
        val calleeCtx = returnNode.asInstanceOf[Final].origin.ctx
        // depending on calculation/specializations being made by callee next line node might be different
        val nextCtx = SpecializationContext.fnCall(origin.ctx, offset, calleeCtx)
        val node = origin.compiler.create(origin.op.next, nextCtx)
        next = next.updated(returnNode, node)
        node
      })

    def invalidate(): Unit = next = Map()
  }

  def call(origin: Origin, offset: FrameOffset, target: String): Call = {
    val node = Link(Origin(origin.compiler, OperationRef(target, 0), SpecializationContext.offset(origin.ctx, offset)))
    Call(origin, node, offset)
  }

  final case class Final(origin: Origin) extends Node.Final
}
