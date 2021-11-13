package com.tabishev.leshy.compiler

import com.tabishev.leshy.node.Node
import com.tabishev.leshy.runtime.Runtime

object BytecodeCompiler {
  def compile(node: Node): Node.Generated =
    new Node.Generated {
      override def runInternal(runtime: Runtime): Node = node.runInternal(runtime)
    }
}
