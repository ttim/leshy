package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.Runtime
import org.objectweb.asm.{ClassWriter, Opcodes}
import org.objectweb.asm.Type

import scala.collection.mutable
import scala.util.Random

object BytecodeCompiler {
  private val classLoader = new InMemoryClassLoader(this.getClass.getClassLoader)
  private val typeGeneratedNode = Type.getType(classOf[Node.Generated])
  private val typeNode = Type.getType(classOf[Node])
  private val typeRuntime = Type.getType(classOf[Runtime])

  def compile(node: Node): Node.Generated = {
    val name = "GenClass_" + Random.nextLong(Long.MaxValue)
    classLoader.add(name, new BytecodeCompiler(node, name).compile())
    val clazz = classLoader.loadClass(name)
    val constructor = clazz.getConstructors().head
    val generated = constructor.newInstance(externalNodes(node).toArray:_*).asInstanceOf[Node.Generated]
    generated
  }

  private def externalNodes(node: Node): Seq[Node] = Seq(node)

  private class InMemoryClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
    val classes: mutable.Map[String, Array[Byte]] = mutable.HashMap()

    def add(name: String, bytes: Array[Byte]): Unit = classes.addOne(name, bytes)

    override def findClass(name: String): Class[_] = {
      val bytes = classes(name)
      defineClass(name, bytes, 0, bytes.length)
    }
  }
}

private class BytecodeCompiler(node: Node, name: String) {
  import BytecodeCompiler._
  private val owner = Type.getObjectType(name)
  private val args = BytecodeCompiler.externalNodes(node)

  def compile(): Array[Byte] = {
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)

    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, typeGeneratedNode.getInternalName, Array())
    writeFields(writer)
    writeConstructor(writer)
    writeRun(writer)
    writer.visitEnd()

    writer.toByteArray
  }

  private def writeFields(writer: ClassWriter): Unit =
    (0 until args.length).foreach { idx =>
      writer.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, "node_" + idx, typeNode.getDescriptor, null, null)
    }

  private def writeConstructor(classWriter: ClassWriter): Unit = {
    val constructorType = Type.getMethodType(Type.VOID_TYPE, Array.fill(args.length)(typeNode):_*)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", constructorType.getDescriptor, null, null)

    writer.visitCode()

    // super()
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitMethodInsn(Opcodes.INVOKESPECIAL, typeGeneratedNode.getInternalName, "<init>", "()V", false)

    (0 until args.length).foreach { idx =>
      // this.node_idx = <arg_idx>
      writer.visitVarInsn(Opcodes.ALOAD, 0)
      writer.visitVarInsn(Opcodes.ALOAD, idx + 1)
      writer.visitFieldInsn(Opcodes.PUTFIELD, owner.getInternalName, "node_" + idx, typeNode.getDescriptor)
    }

    // return
    writer.visitInsn(Opcodes.RETURN)
    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }

  private def writeRun(classWriter: ClassWriter): Unit = {
    val methodType = Type.getMethodType(typeNode, typeRuntime)
    val writer = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "runInternal", methodType.getDescriptor, null, null)

    writer.visitCode()

    // return this.node_0
    writer.visitVarInsn(Opcodes.ALOAD, 0)
    writer.visitFieldInsn(Opcodes.GETFIELD, owner.getInternalName, "node_0", typeNode.getDescriptor)
    writer.visitInsn(Opcodes.ARETURN)

    writer.visitMaxs(1, 1)
    writer.visitEnd()
  }
}
