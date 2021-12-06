package com.tabishev.leshy.node

import com.tabishev.leshy.runtime.{FrameOffset, Runtime}
import org.objectweb.asm.{Label, MethodVisitor, MethodWriter}

import scala.collection.mutable

trait Node

object Node {
  trait Run extends Node {
    def command: Command

    def next: Node
  }

  trait Branch extends Node {
    def condition: Condition

    def ifTrue: Node
    def ifFalse: Node
  }

  trait Call extends Node {
    def offset: FrameOffset

    def call: Node
    def next(returnNode: Node.Final): Node
  }

  trait Final extends Node
}
