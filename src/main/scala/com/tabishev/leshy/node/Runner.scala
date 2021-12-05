package com.tabishev.leshy.node

import com.tabishev.leshy.ast.Bytes
import com.tabishev.leshy.bytecode.BytecodeExpression.*
import com.tabishev.leshy.bytecode.{BranchModifier, BytecodeExpression, branch, statement}
import com.tabishev.leshy.runtime.{MemoryRef, Runtime}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

object Runner {
  private val Debug: Boolean = false

  def create(ctx: RunnerCtx, node: Node): Runner = node match {
    case node: Node.Run if node.command == Command.Noop => ctx.create(node.next)
    case node: Node.Run => new CommandRunner(ctx, node)
    case node: Node.Branch => new BranchRunner(ctx, node)
    case node: Node.Call => new CallRunner(ctx, node)
    case node: Node.Final => new FinalRunner(ctx, node)
  }
}

sealed abstract class Runner {
  val ctx: RunnerCtx
  val node: Node

  final def runFully(runtime: Runtime): FinalRunner = {
    var runner: Runner = this
    while (!runner.isInstanceOf[FinalRunner]) {
      debug("start run")
      val nextRunner = runner.runInternal(runtime)
      debug("finish run")
      runner = nextRunner
    }

    runner.asInstanceOf[FinalRunner]
  }

  def runInternal(runtime: Runtime): Runner

  private inline def debug(inline msg: => String): Unit =
    if (Runner.Debug) println(s"[${toString}]: $msg")
}

trait RunnerCtx {
  def create(node: Node): Runner
}

class CommandRunner(val ctx: RunnerCtx, val node: Node.Run) extends Runner {
  private val impl = Runners.command(node.command)
  private var next: Runner = null

  override def runInternal(runtime: Runtime): Runner = {
    impl.run(runtime)
    if (next == null) next = ctx.create(node.next)
    next
  }
}

class BranchRunner(val ctx: RunnerCtx, val node: Node.Branch) extends Runner {
  private val impl = Runners.condition(node.condition)
  private var ifTrue: Runner = null
  private var ifFalse: Runner = null

  override def runInternal(runtime: Runtime): Runner = {
    if (impl.run(runtime)) {
      if (ifTrue == null) ifTrue = ctx.create(node.ifTrue)
      ifTrue
    } else {
      if (ifFalse == null) ifFalse = ctx.create(node.ifFalse)
      ifFalse
    }
  }
}

class CallRunner(val ctx: RunnerCtx, val node: Node.Call) extends Runner {
  private val offset = node.offset.get
  private var call: Runner = null
  // todo: intern final nodes and make this map from interned id?
  private[node] var next: Map[Node.Final, Runner] = Map.empty

  override def runInternal(runtime: Runtime): Runner = {
    runtime.stack.moveFrame(offset)
    if (call == null) call = ctx.create(node.call)
    val finalRunner = call.runFully(runtime)
    runtime.stack.moveFrame(-offset)
    next.getOrElse(finalRunner.node, {
      next = next.updated(finalRunner.node, ctx.create(node.next(finalRunner.node)))
      next(finalRunner.node)
    })
  }
}

class FinalRunner(val ctx: RunnerCtx, val node: Node.Final) extends Runner {
  override def runInternal(runtime: Runtime): Runner = throw new IllegalStateException()
}

abstract class GeneratedRunner extends Runner

abstract class CommandImpl {
  def run(runtime: Runtime): Unit
}

abstract class ConditionImpl {
  def run(runtime: Runtime): Boolean
}
