package com.tabishev.leshy.node

object Inliner {
  def inlineIndirect(node: Node): Node =
    tryInline(Map(), node).getOrElse(node)

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

  private def tryInline(inProcess: Map[Node, Holder], node: Node): Option[Node] =
    if (inProcess.contains(node)) Some(inProcess(node)) else {
      val holder = new Holder()
      val nextInProcess = inProcess + ((node, holder))
      val inlined: Option[Node] = node match {
        case indirect: Node.Indirect =>
          indirect.tryResolve() match {
            case Some(resolved) => Some(tryInline(nextInProcess, resolved).getOrElse(resolved))
            case None => None
          }
        case run: Node.Run =>
          tryInline(nextInProcess, run.next).map { inlinedNext =>
            run.replace(withNext = inlinedNext)
          }
        case branch: Node.Branch =>
          val inlinedTrue = tryInline(nextInProcess, branch.ifTrue)
          val inlinedFalse = tryInline(nextInProcess, branch.ifFalse)
          if (inlinedTrue.isDefined || inlinedFalse.isDefined)
            Some(branch.replace(
              withIfTrue = inlinedTrue.getOrElse(branch.ifTrue),
              withIfFalse = inlinedFalse.getOrElse(branch.ifFalse)
            ))
          else
            None
        case call: Node.Call =>
          tryInline(nextInProcess, call.call).map { inlinedCall =>
            call.replace(withCall = inlinedCall)
          }
        case _: Node.Final =>
          None
        case _: Node.Generated =>
          None
      }
      holder.resolved = inlined.getOrElse(node)
      inlined
    }
}
