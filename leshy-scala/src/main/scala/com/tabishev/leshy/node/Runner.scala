package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.StackMemory

object Runner {
  private val Debug: Boolean = false

  def create(ctx: RunnerCtx, node: Node): Runner = node.get() match {
    case kind: Node.Run => new CommandRunner(ctx, node, kind)
    case kind: Node.Branch => new BranchRunner(ctx, node, kind)
    case kind: Node.Call => new CallRunner(ctx, node, kind)
    case Node.Final => new FinalRunner(ctx, node)
  }
}

sealed abstract class Runner {
  val ctx: RunnerCtx
  val node: Node

  def invalidate(): Unit

  final def runFully(stack: StackMemory): FinalRunner = {
    var runner: Runner = this
    while (!runner.isInstanceOf[FinalRunner]) {
      debug("start run")
      val nextRunner = runner.runInternal(stack)
      debug("finish run")
      runner = nextRunner
    }

    runner.asInstanceOf[FinalRunner]
  }

  def runInternal(stack: StackMemory): Runner

  private def debug(msg: String): Unit =
    if (Runner.Debug) println(s"[${toString}]: $msg")
}

trait RunnerCtx {
  def create(node: Node): Runner
}

class CommandRunner(val ctx: RunnerCtx, val node: Node, val kind: Node.Run) extends Runner {
  private val impl = Runners.command(kind.command)
  private var next: Runner = null

  override def runInternal(stack: StackMemory): Runner = {
    impl.run(stack)
    if (next == null) next = ctx.create(kind.next)
    next
  }

  override def invalidate(): Unit = {
    next = null
  }
}

class BranchRunner(val ctx: RunnerCtx, val node: Node, val kind: Node.Branch) extends Runner {
  private val impl = Runners.condition(kind.condition)
  private var ifTrue: Runner = null
  private var ifFalse: Runner = null

  override def runInternal(stack: StackMemory): Runner = {
    if (impl.run(stack)) {
      if (ifTrue == null) ifTrue = ctx.create(kind.ifTrue)
      ifTrue
    } else {
      if (ifFalse == null) ifFalse = ctx.create(kind.ifFalse)
      ifFalse
    }
  }

  override def invalidate(): Unit = {
    ifTrue = null
    ifFalse = null
  }
}

class CallRunner(val ctx: RunnerCtx, val node: Node, val kind: Node.Call) extends Runner {
  private val offset = kind.offset.get
  private var call: Runner = null
  // todo: intern final nodes and make this map from interned id?
  private[node] var next: Map[FinalRunner, Runner] = Map.empty

  override def runInternal(stack: StackMemory): Runner = {
    stack.moveFrame(offset)
    if (call == null) call = ctx.create(kind.call)
    val finalRunner = call.runFully(stack)
    stack.moveFrame(-offset)
    nextRunner(finalRunner)
  }

  def nextRunner(finalRunner: FinalRunner): Runner =
    next.getOrElse(finalRunner, {
      next = next.updated(finalRunner, ctx.create(kind.next(finalRunner.node)))
      next(finalRunner)
    })

  override def invalidate(): Unit = {
    call = null
    next = Map.empty
  }
}

class FinalRunner(val ctx: RunnerCtx, val node: Node) extends Runner {
  override def runInternal(stack: StackMemory): Runner = throw new IllegalStateException()

  override def invalidate(): Unit = ()
}

abstract class GeneratedRunner extends Runner {
  override def invalidate(): Unit =
    this.getClass.getDeclaredFields.foreach {
      case field if field.getType.isAssignableFrom(classOf[Runner]) =>
        val prev = field.get(this).asInstanceOf[Runner]
        field.set(this, prev.ctx.create(prev.node))
      case _ => // do nothing
    }
}

abstract class CommandImpl {
  def run(stack: StackMemory): Unit
}

abstract class ConditionImpl {
  def run(stack: StackMemory): Boolean
}
