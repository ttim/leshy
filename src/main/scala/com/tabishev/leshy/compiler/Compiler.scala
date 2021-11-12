package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.common.ConstInterpreter
import com.tabishev.leshy.loader.FnLoader
import com.tabishev.leshy.node.{Node, NodeUtils}
import com.tabishev.leshy.runtime.{Consts, FnSpec, Runtime, StackMemory}

import scala.collection.mutable

final class Compiler(val loader: FnLoader, val runtime: Runtime, val debugEnabled: Boolean) {
  private val nodes = mutable.HashMap[(OperationRef, SpecializationContext), Node]()

  def run[T, V](spec: FnSpec[T, V])(input: T): V = {
    assert(runtime.stack.isEmpty())
    val inputObj = spec.input(input)
    runtime.stack.append(inputObj.bytes)
    val initialContext = SpecializationContext.from(runtime.stack.frameSize(), inputObj.consts)
    create(OperationRef(spec.fn, 0), initialContext).run(runtime)
    assert(runtime.stack.getFrameOffset() == 0)
    val output = Bytes.fromBytes(runtime.stack.currentStackFrame())
    runtime.stack.shrink(output.length())
    spec.output(output)
  }

  private[compiler] def create(op: OperationRef, ctx: SpecializationContext): Node = {
    debug(op, ctx, "start create")

    val nodeKey = (op, ctx)
    if (nodes.contains(nodeKey)) return nodes(nodeKey)

    val contextFn = loader.load(op.fn).get
    runtime.symbols.register(contextFn)

    val node = NodeFactory.create(this, runtime.symbols, ctx, op, contextFn)

    nodes.put(nodeKey, node)

    if (nodes.size > 100) debug(op, ctx, s"too many created nodes ${nodes.size}")

    debug(op, ctx, "finish create")

    node
  }

  def optimize(): Unit = {
    nodes.values.foreach(NodeUtils.inlineIndirectNode)
  }

  private def debug(op: OperationRef, ctx: SpecializationContext, msg: String, force: Boolean = false): Unit =
    if (debugEnabled || force) {
      val fnCtx = loader.load(op.fn).get
      println(s"${runtime.stack.frameToString(ctx.get()._2)}, ${op.toString(fnCtx)}: $msg")
    }
}
