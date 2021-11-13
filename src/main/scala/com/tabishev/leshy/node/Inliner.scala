package com.tabishev.leshy.node

object Inliner {
  def inlineIndirect(node: Node): Node = inlineIndirect(Map(), node)

  private final class Holder extends Node.Indirect {
    var resolved: Node = null

    override def resolve(): Node = {
      assert(resolved != null)
      resolved
    }

    override def tryResolve(): Option[Node] = {
      assert(resolved != null)
      Some(resolved)
    }
  }

  private def inlineIndirect(inProcess: Map[Node, Holder], node: Node): Node =
    if (inProcess.contains(node)) inProcess(node) else {
      val holder = new Holder()
      val nextInProcess = inProcess + ((node, holder))
      val inlined: Node = node match {
        case indirect: Node.Indirect =>
          indirect.tryResolve() match {
            case Some(resolved) => inlineIndirect(nextInProcess, resolved)
            case None => indirect
          }
        case run: Node.Run =>
          run.copy(next = inlineIndirect(nextInProcess, run.next))
        case branch: Node.Branch =>
          branch.copy(ifTrue = inlineIndirect(nextInProcess, branch.ifTrue), ifFalse = inlineIndirect(nextInProcess, branch.ifFalse))
        case call: Node.Call =>
          call.copy(call = inlineIndirect(nextInProcess, call.call))
        case _: Node.Final =>
          node
        case _: Node.Generated =>
          node
      }
      holder.resolved = inlined
      inlined
    }
}
