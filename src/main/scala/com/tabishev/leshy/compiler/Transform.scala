package com.tabishev.leshy.compiler

trait Transform {
  def transformNodeInScope(node: Node, replaces: (Node | NodeSupplier) => NodeSupplier): Node
  def transformNodeOutOfScope(node: Node): Node
  def transformNodeSupplier(nodeSupplier: NodeSupplier): NodeSupplier
}

object Transform {
  def apply(nodes: Seq[Node], transform: Transform): Map[Node, Node] = {
    val optimized: Map[Node, NodeHolder] = nodes.map { node => (node, new NodeHolder()) }.toMap
    val replaces: (Node | NodeSupplier) => NodeSupplier = {
      case node: Node => optimized.getOrElse(node, NodeSupplier.Const(transform.transformNodeOutOfScope(node)))
      case supplier: NodeSupplier => transform.transformNodeSupplier(supplier)
    }

    optimized.foreach { case (node, holder) =>
      holder.node = transform.transformNodeInScope(node, replaces)
    }

    optimized.map { case (node, holder) => (node, holder.node) }
  }
}

object DontMarkConsts extends Transform {
  override def transformNodeInScope(node: Node, replaces: (Node | NodeSupplier) => NodeSupplier): Node = {
    // we need to disable context checks, otherwise it's going to fail
    val optimizedOptions = node.options.copy(checkContext = false)
    node match {
      case run: Node.Run => run.updated(optimizedOptions, markConsts = false, replaces)
      case branch: Node.Branch => branch.updated(optimizedOptions, replaces)
      case call: Node.Call => call.updated(optimizedOptions, replaces)
      case finalNode: Node.Final => finalNode.updated(optimizedOptions)
    }
  }

  override def transformNodeOutOfScope(node: Node): Node = ???

  override def transformNodeSupplier(nodeSupplier: NodeSupplier): NodeSupplier = ???
}
