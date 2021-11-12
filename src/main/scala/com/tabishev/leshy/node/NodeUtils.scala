package com.tabishev.leshy.node

object NodeUtils {
  def inlineLazyNode(node: Node): Unit = node match {
    case run: Node.Run =>
      inlineLazyNode(() => run.next, node => run.next = node)
    case branch: Node.Branch =>
      inlineLazyNode(() => branch.ifTrue, node => branch.ifTrue = node)
      inlineLazyNode(() => branch.ifFalse, node => branch.ifFalse = node)
    case call: Node.Call =>
      inlineLazyNode(() => call.call, node => call.call = node)
    case _ => // do nothing
  }

  private def inlineLazyNode(get: () => Node, set: Node => Unit): Unit =
    get() match {
      case next: Node.LazyNode =>
        if (next.node != null) set(next.node)
      case _ => // do nothing
    }
}
