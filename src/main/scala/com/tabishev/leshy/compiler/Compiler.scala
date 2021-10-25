package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{FnSpec, Runtime, StackMemory}

import scala.collection.mutable

class Compiler(val loader: RoutineLoader, val runtime: Runtime, val debugEnabled: Boolean) {
  private val constInterpreter = ConstInterpreter(runtime)
  private val registeredFns = mutable.HashSet[String]()
  private val nodes = mutable.HashMap[(OperationRef, SpecializationContext), Node]()

  private[compiler] var frozen: Boolean = false

  def freeze(): Unit = {
    frozen = true
    nodes.values.foreach {
      case node: UpdatesConst => node.markConst = false
      case _ => // do nothing
    }
  }

  private[compiler] inline def debug(inline op: OperationRef, inline ctx: SpecializationContext, inline msg: String, force: Boolean = false): Unit =
    if (debugEnabled || force) {
      val fnCtx = loader.load(op.fn).get
      println(s"${runtime.stack.frameToString}, ${op.toString(fnCtx)}: $msg")
    }

  def run[T, V](spec: FnSpec[T, V])(input: T): V =
    spec.output(run(spec.fn)(stack => spec.input(input, stack)))

  def run(fn: String)(init: StackMemory => Unit): Bytes = {
    assert(runtime.stack.frameOffset == 0 && runtime.stack.size == 0)
    init(runtime.stack)
    create(OperationRef(fn, 0), SpecializationContext.current(runtime)).run()
    assert(runtime.stack.frameOffset == 0)
    val output = Bytes.fromBytes(runtime.stack.getCurrentStackFrame())
    runtime.stack.shrink(output.length())
    output
  }

  private[compiler] def create(op: OperationRef, ctx: SpecializationContext): Node = {
    if (!frozen) // & debug?
      assert(ctx == SpecializationContext.current(runtime))

    debug(op, ctx, "start create")

    val nodeKey = (op, ctx)
    if (nodes.contains(nodeKey)) return nodes(nodeKey)

    val contextFn = loader.load(op.fn).get
    if (frozen) throw new IllegalStateException(s"nodes are frozen, can't create node for ${op.toString(contextFn)} with ${runtime.stack.frameToString}")

    if (!registeredFns.contains(op.fn)) {
      registeredFns.add(op.fn)
      runtime.symbols.register(op.fn)
      contextFn.labels.foreach { case (label, _) => runtime.symbols.register(label) }
    }

    val node = NodeFactory.create(this, constInterpreter, ctx, op, contextFn)

    nodes.put(nodeKey, node)

    if (nodes.size > 100) debug(op, ctx, s"too many created nodes ${nodes.size}")

    debug(op, ctx, "finish create")

    node
  }
}
