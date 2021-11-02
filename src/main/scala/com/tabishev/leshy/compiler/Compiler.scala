package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.FnLoader
import com.tabishev.leshy.runtime.{FnSpec, Runtime, StackMemory}

import scala.collection.mutable

final class Compiler(val loader: FnLoader, val runtime: Runtime, val debugEnabled: Boolean) {
  private val constInterpreter = ConstInterpreter(runtime)
  private val nodes = mutable.HashMap[(OperationRef, SpecializationContext), Node]()

  def run[T, V](spec: FnSpec[T, V])(input: T): V =
    spec.output(run(spec.fn)(stack => spec.input(input, stack)))

  def run(fn: String)(init: Runtime => Unit): Bytes = {
    assert(runtime.stack.isEmpty())
    init(runtime)
    create(OperationRef(fn, 0), SpecializationContext.current(runtime)).run(runtime)
    assert(runtime.stack.getFrameOffset() == 0)
    val output = Bytes.fromBytes(runtime.stack.currentStackFrame())
    runtime.stack.shrink(output.length())
    output
  }

  private[compiler] def create(op: OperationRef, ctx: SpecializationContext): Node = {
    debug(op, ctx, "start create")

    val nodeKey = (op, ctx)
    if (nodes.contains(nodeKey)) return nodes(nodeKey)

    val contextFn = loader.load(op.fn).get
    runtime.symbols.register(contextFn)

    val node = NodeFactory.create(this, constInterpreter, ctx, op, contextFn)

    nodes.put(nodeKey, node)

    if (nodes.size > 100) debug(op, ctx, s"too many created nodes ${nodes.size}")

    debug(op, ctx, "finish create")

    node
  }

  def optimize(): Unit = {
    transform(DontMarkConsts)
  }

  private def transform(transform: Transform): Unit = {
    val transformed = Transform.apply(nodes.values.toSeq, transform)
    nodes.mapValuesInPlace { case (_, node) => transformed(node) }
  }

  private def debug(op: OperationRef, ctx: SpecializationContext, msg: String, force: Boolean = false): Unit =
    if (debugEnabled || force) {
      val fnCtx = loader.load(op.fn).get
      println(s"${runtime.stack.frameToString(runtime.consts.get())}, ${op.toString(fnCtx)}: $msg")
    }
}
