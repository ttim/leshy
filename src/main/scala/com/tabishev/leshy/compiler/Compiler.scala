package com.tabishev.leshy.compiler

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.interpreter.ConstInterpreter
import com.tabishev.leshy.loader.RoutineLoader
import com.tabishev.leshy.runtime.{FnSpec, Runtime, StackMemory}

import scala.collection.mutable

final class Compiler(val loader: RoutineLoader, val runtime: Runtime, val debugEnabled: Boolean) {
  private val constInterpreter = ConstInterpreter(runtime)
  private val registeredFns = mutable.HashSet[String]()
  private val nodes = mutable.HashMap[(OperationRef, SpecializationContext), Node]()

  def optimize(): Unit = {
     // as result of optimize we want to disable checkContext option for all nodes, and const marking,
     // but restore mark context in cases when next node isn't yet supplied
  }

  private def debug(op: OperationRef, ctx: SpecializationContext, msg: String, force: Boolean = false): Unit =
    if (debugEnabled || force) {
      val fnCtx = loader.load(op.fn).get
      println(s"${runtime.stack.frameToString}, ${op.toString(fnCtx)}: $msg")
    }

  def run[T, V](spec: FnSpec[T, V])(input: T): V =
    spec.output(run(spec.fn)(stack => spec.input(input, stack)))

  def run(fn: String)(init: StackMemory => Unit): Bytes = {
    assert(runtime.stack.isEmpty())
    init(runtime.stack)
    create(OperationRef(fn, 0), SpecializationContext.current(runtime)).run(runtime)
    assert(runtime.stack.getFrameOffset() == 0)
    val output = Bytes.fromBytes(runtime.stack.currentStackFrame())
    runtime.stack.shrink(output.length())
    output
  }

  private[compiler] def create(op: OperationRef, ctx: SpecializationContext): Node = {
    assert(ctx == SpecializationContext.current(runtime))

    debug(op, ctx, "start create")

    val nodeKey = (op, ctx)
    if (nodes.contains(nodeKey)) return nodes(nodeKey)

    val contextFn = loader.load(op.fn).get

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
