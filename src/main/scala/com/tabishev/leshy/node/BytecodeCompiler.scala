package com.tabishev.leshy.node

import com.tabishev.leshy.bytecode.*
import com.tabishev.leshy.bytecode.BytecodeExpression.*
import com.tabishev.leshy.runtime.{Runtime, StackMemory}
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
  private val typeRuntime = Type.getType(classOf[Runtime])

  def blah(): Unit = System.out.println("blah!")

  val RuntimeExpression: BytecodeExpression = local[Runtime](1)
  val StackExpression: BytecodeExpression = local[StackMemory](2)

  // prepare dest
  if (Files.exists(dest)) {
    Files.walk(dest)
      .sorted(Comparator.reverseOrder())
      .map(_.toFile)
      .forEach(_.delete)
  }
  Files.createDirectory(dest)

  def compile(ctx: RunnerCtx, stats: Stats, node: Node): GeneratedRunner =
    BytecodeCompiler(node, "GenClass_" + Random.nextLong(Long.MaxValue), ctx, stats).compile()
}

private class BytecodeCompiler(node: Node, name: String, ctx: RunnerCtx, stats: Stats) {
  import BytecodeCompiler._
  private val owner = Type.getObjectType(name)
  private val (internal, external) = traverse(node)
  private val nodeToArg = external.zipWithIndex.map { (node, idx) => (node, argName(idx)) }.toMap

  def compile(): GeneratedRunner = {
    //    println(s"compile $node into $name")
    Files.write(dest.resolve(name + ".class"), generate())
    val clazz = classLoader.loadClass(name)
    val constructor = clazz.getConstructors().head
    val generated = constructor.newInstance(external.map(ctx.create).toArray:_*).asInstanceOf[GeneratedRunner]
    generated
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
      writer.visitField(Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL, argName(idx), typeRunner.getDescriptor, null, null)
    }

  private def writeConstructor(classWriter: ClassWriter): Unit = {
    val constructorType = Type.getMethodType(Type.VOID_TYPE, Array.fill(external.length)(typeRunner):_*)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorType.getDescriptor, null, null)

    writer.visitCode()

    writer.statement(InvokeSuper(classOf[GeneratedRunner]))
    external.indices.foreach { idx =>
      writer.putField(Field(isStatic = false, argName(idx), owner, typeRunner), BytecodeExpression.local[Runner](idx + 1))
    }

    // return
    writer.visitInsn(Opcodes.RETURN)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def writeRun(classWriter: ClassWriter): Unit = {
    val methodType = Type.getMethodType(typeRunner, typeRuntime)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "runInternal", methodType.getDescriptor, null, null)

    val start = new Label()
    val finish = new Label()

    writer.visitCode()
    writer.visitLocalVariable("stack", Type.getDescriptor(classOf[StackMemory]), null, start, finish, 2)
    writer.visitLocalVariable("finalTmp", Type.getDescriptor(classOf[Node.Final]), null, start, finish, 3)

    writer.visitLabel(start)
    writer.storeVar(2, BytecodeExpression.invokeVirtual(classOf[Runtime], "stack", RuntimeExpression))

    val nodes = internal ++ external
    assert(internal.intersect(external).isEmpty)
    val labels = nodes.map { node => (node, new Label()) }.toMap

    def label(node: Node): Label = labels(node)
    def jump(fromLine: Int, target: Node): Unit =
      if (fromLine != nodes.length -1 && target == nodes(fromLine + 1)) {
        // do nothing, jump to next instruction
      } else {
        writer.visitJumpInsn(Opcodes.GOTO, label(target))
      }

    nodes.zipWithIndex.foreach { (node, line) =>
      // todo: don't write label per each line?
      writer.visitLabel(labels(node))

      node match {
        case _ if external.contains(node) =>
          writer.ret(Field(isStatic = false, nodeToArg(node), owner, typeRunner))
        case run: com.tabishev.leshy.node.Node.Run =>
          Generate.command(run.command, writer)
          jump(line, run.next)
        case branch: com.tabishev.leshy.node.Node.Branch =>
          Generate.condition(branch.condition, writer, label(branch.ifTrue))
          jump(line, branch.ifFalse)
        case call: com.tabishev.leshy.node.Node.Call =>
          throw new IllegalStateException("call nodes can't be internal")
          // todo
//          val callNodeExpression = Cast(externalExpression(call), classOf[Node.Call])
//          writer.statement(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(call.offset.get)))
//          writer.storeVar(3, BytecodeExpression.invokeVirtual(classOf[Node], "run", externalExpression(call.call), RuntimeExpression))
//          writer.statement(invokeVirtual(classOf[StackMemory], "moveFrame", StackExpression, const(-call.offset.get)))
//
//          val next = stats.recordedCallFinals(call)
//          if (next.size == 1) {
//            val (finalNode, nextNode) = next.head
//            // final node equals to expected final node
//            // seems like this equals is rarely true. why?!?
//            writer.push(invokeVirtual(classOf[Object], "equals", local[Node.Final](3), externalExpression(finalNode)))
//            writer.visitJumpInsn(Opcodes.IFGT, label(resolve(nextNode)))
//          }
//          // fallback
//          writer.ret(BytecodeExpression.invokeVirtual(classOf[Node.Call], "next", callNodeExpression, local[Node.Final](3)))
        case _: com.tabishev.leshy.node.Node.Final =>
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

    def go(node: Node): Unit = node match {
      case _ if !stats.isExecuted(node) =>
        external.add(node)
      case _ if internal.contains(node) || external.contains(node) =>
        // do nothing
      case run: com.tabishev.leshy.node.Node.Run =>
        internal.add(run)
        go(run.next)
      case branch: com.tabishev.leshy.node.Node.Branch =>
        internal.add(branch)
        go(branch.ifFalse)
        go(branch.ifTrue)
      case call: com.tabishev.leshy.node.Node.Call =>
        external.add(call)
//        val next = stats.recordedCallFinals(call)
//        if (next.size == 1) {
//          val (finalNode, nextNode) = next.head
//          internal.add(call)
//          external.add(call.call)
//          external.add(finalNode)
//          go(nextNode)
//        }
      case _: com.tabishev.leshy.node.Node.Final => external.add(node)
    }
    go(root)

    (internal.toSeq, external.toSeq)
  }

  def argName(idx: Int): String = s"runner_$idx"
}
