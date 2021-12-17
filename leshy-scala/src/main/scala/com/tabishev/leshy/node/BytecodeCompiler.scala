package com.tabishev.leshy.node

import com.tabishev.leshy.bytecode._
import com.tabishev.leshy.bytecode.Expression._
import com.tabishev.leshy.bytecode.Statement._
import com.tabishev.leshy.runtime.StackMemory
import com.tabishev.leshy.bytecode.Statement.WriterExtension

import org.objectweb.asm.{ClassWriter, Label, Opcodes, Type}

import java.net.URLClassLoader
import java.nio.file.Files
import java.util.Comparator
import scala.collection.mutable
import scala.util.Random

object BytecodeCompiler {
  private val dest = new java.io.File("generated").toPath
  private val classLoader = new URLClassLoader(Array(dest.toUri.toURL), this.getClass.getClassLoader)
  private val typeGeneratedRunner = Type.getType(classOf[GeneratedRunner])
  private val typeRunner = Type.getType(classOf[Runner])
  private val typeStack = Type.getType(classOf[StackMemory])

  val StackExpression: Expression = local[StackMemory](1)

  // prepare dest
  if (Files.exists(dest)) {
    Files.walk(dest)
      .sorted(Comparator.reverseOrder())
      .map(_.toFile)
      .forEach(_.delete)
  }
  Files.createDirectory(dest)

  def compile(ctx: RunnerCtx, stats: Stats, node: Node): RunnerCtx => GeneratedRunner =
    new BytecodeCompiler(node, "GenClass_" + Random.nextLong(Long.MaxValue), stats).compile()
}

private class BytecodeCompiler(node: Node, name: String, stats: Stats) {
  import BytecodeCompiler._
  private val owner = Type.getObjectType(name)
  private val (internal, external) = traverse(node)
  private val nodeToArg = external.zipWithIndex.map { case (node, idx) => (node, argName(idx)) }.toMap

  def compile(): RunnerCtx => GeneratedRunner = {
    //    println(s"compile $node into $name")
    Files.write(dest.resolve(name + ".class"), generate())
    val clazz = classLoader.loadClass(name)
    val constructor = clazz.getConstructors().head

    ctx => constructor.newInstance(external.map(ctx.create).toArray:_*).asInstanceOf[GeneratedRunner]
  }

  def generate(): Array[Byte] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)

    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, typeGeneratedRunner.getInternalName, Array())
    writeFields(writer)
    writeConstructor(writer)
    writeRun(writer)
    writer.visitEnd()

    writer.toByteArray
  }

  private def writeFields(writer: ClassWriter): Unit =
    external.indices.foreach { idx =>
      writer.visitField(Opcodes.ACC_PUBLIC, argName(idx), typeRunner.getDescriptor, null, null)
    }

  private def writeConstructor(classWriter: ClassWriter): Unit = {
    val constructorType = Type.getMethodType(Type.VOID_TYPE, Array.fill(external.length)(typeRunner):_*)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorType.getDescriptor, null, null)

    writer.visitCode()

    writer.write(Expr(InvokeSuper(classOf[GeneratedRunner])))
    external.indices.foreach { idx =>
      writer.write(PutField(Field(isStatic = false, argName(idx), owner, typeRunner), Expression.local[Runner](idx + 1)))
    }

    // return
    writer.write(Return(void()))
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def writeRun(classWriter: ClassWriter): Unit = {
    val methodType = Type.getMethodType(typeRunner, typeStack)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "runInternal", methodType.getDescriptor, null, null)

    val start = new Label()
    val finish = new Label()

    writer.visitCode()
    writer.visitLocalVariable("finalTmp", Type.getDescriptor(classOf[FinalRunner]), null, start, finish, 2)

    writer.visitLabel(start)

    val nodes = internal ++ external.diff(internal)
    val labels = nodes.map { node => (node, new Label()) }.toMap

    def externalRunner(node: Node): Expression = Field(isStatic = false, nodeToArg(node), owner, typeRunner)
    def label(node: Node): Label = labels(node)
    def jump(fromLine: Int, target: Node): Unit =
      if (fromLine != nodes.length -1 && target == nodes(fromLine + 1)) {
        // do nothing, jump to next instruction
      } else {
        writer.write(Goto(label(target)))
      }

    nodes.zipWithIndex.foreach { case (node, line) =>
      // todo: don't write label per each line?
      writer.visitLabel(labels(node))

      node.get() match {
        case _ if line >= internal.size =>
          // external node
          writer.write(Return(externalRunner(node)))
        case run: Node.Run =>
          Generate.command(run.command, writer)
          jump(line, run.next)
        case branch: Node.Branch =>
          Generate.condition(branch.condition, writer, label(branch.ifTrue))
          jump(line, branch.ifFalse)
        case call: Node.Call =>
          writer.write(Expr(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(call.offset.get))))
          writer.write(StoreVar(2, Expression.invokeVirtual(classOf[Runner], "runFully", externalRunner(call.call), StackExpression)))
          writer.write(Expr(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(-call.offset.get))))

          val next = stats.recordedCallFinals(node)
          if (next.size == 1) {
            val (finalNode, nextNode) = next.head
            writer.write(BooleanBranch(invokeVirtual(classOf[Object], "equals", local[FinalRunner](2), externalRunner(finalNode)), label(nextNode)))
          }

          // fallback
          val callRunnerExpression = Cast(externalRunner(node), classOf[CallRunner])
          writer.write(Return(invokeVirtual(classOf[CallRunner], "nextRunner", callRunnerExpression, local[FinalRunner](2))))
        case Node.Final =>
          throw new IllegalStateException("final nodes can't be internal")
      }
    }

    writer.visitLabel(finish)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  def traverse(root: Node): (Seq[Node], Seq[Node]) = {
    val internal = mutable.LinkedHashSet[Node]()
    val external = mutable.LinkedHashSet[Node]()

    def go(node: Node): Unit = node.get() match {
      case _ if !stats.isExecuted(node) =>
        external.add(node)
      case _ if internal.contains(node) || external.contains(node) =>
        // do nothing
      case run: Node.Run =>
        internal.add(node)
        go(run.next)
      case branch: Node.Branch =>
        internal.add(node)
        go(branch.ifFalse)
        go(branch.ifTrue)
      case call: Node.Call =>
        // call needs to be in external for fallbacks
        external.add(node)
        // but also it is generateable if recordedCallFinals contains only one elements
        val next = stats.recordedCallFinals(node)
        if (next.size == 1) {
          val (finalNode, nextNode) = next.head
          internal.add(node)
          external.add(call.call)
          external.add(finalNode)
          go(nextNode)
        }
      case Node.Final =>
        external.add(node)
    }
    go(root)

    (internal.toSeq, external.toSeq)
  }

  def argName(idx: Int): String = s"runner_$idx"
}
