package com.tabishev.leshy.node

object NodeUtils {
  def inlineIndirectNode(node: Node): Unit = node match {
    case run: Node.Run =>
      inlineIndirectNode(() => run.next, node => run.next = node)
    case branch: Node.Branch =>
      inlineIndirectNode(() => branch.ifTrue, node => branch.ifTrue = node)
      inlineIndirectNode(() => branch.ifFalse, node => branch.ifFalse = node)
    case call: Node.Call =>
      inlineIndirectNode(() => call.call, node => call.call = node)
      call.next = call.next.map { (key, value) =>
        (key, value match {
          case indirect: Node.Indirect if indirect.node != null => indirect.node
          case other => other
        })
      }
    case indirect: Node.Indirect =>
      inlineIndirectNode(() => indirect.node, node => indirect.node = node)
    case _: Node.Generated =>
      // do nothing
    case _: Node.Final =>
      // do nothing
  }

  private def inlineIndirectNode(get: () => Node, set: Node => Unit): Unit =
    get() match {
      case next: Node.Indirect if next.node != null => set(next.node)
      case _ => // do nothing
    }
}
