package com.tabishev.leshy.parser

import com.tabishev.leshy.ast.{Address, Const, Operation, Subroutine}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.Charset

object TextParser {
  def parse(text: String): Map[String, Subroutine] =
    ("\n" + text).split("\ndef ").tail.map(parseSubroutine).map { r => (r.name, r) }.toMap

  private def parseSubroutine(text: String): Subroutine = {
    val lines = text.split("\n")
    Subroutine(lines.head, lines.toSeq.tail.flatMap(parseOperation))
  }

  private def parseOperation(text: String): Option[Operation] =
    cleanOperation(text).split(" ").toSeq match {
      // stack
      case Seq("extend", length) =>
        Some(Operation.Extend(parseConst(length)))
      case Seq("shrink", length) =>
        Some(Operation.Shrink(parseConst(length)))
      case Seq("check_size", length) =>
        Some(Operation.CheckSize(parseConst(length)))
      case Seq("append", bytes) =>
        Some(Operation.Append(parseConst(bytes)))

      // branch
      case Seq("branch", modifier, length, op1, op2, dest) =>
        Some(Operation.Branch(parseConst(modifier), parseConst(length), parseConstOrAddress(op1), parseConstOrAddress(op2), parseConst(dest)))

      // call
      case Seq("call", offset, target) =>
        Some(Operation.Call(parseConst(offset), parseConst(target)))

      // memory
      case Seq("copy", length, src, dst) =>
        Some(Operation.Copy(parseConst(length), parseConstOrAddress(src), parseAddress(dst)))

      // arithmetic
      case Seq("add", length, op1, op2, dst) =>
        Some(Operation.Add(parseConst(length), parseConstOrAddress(op1), parseConstOrAddress(op2), parseAddress(dst)))

      // "syscalls"
      case Seq("print_int", length, src) =>
        Some(Operation.PrintInt(parseConst(length), parseAddress(src)))

      case Seq("") =>
        None
      case _ =>
        throw new IllegalArgumentException(s"can't parse operation '${cleanOperation(text)}'")
    }

  private def cleanOperation(operation: String): String = {
    val noComment = if (operation.contains(";")) operation.substring(0, operation.indexOf(";")) else operation
    noComment.trim
  }

  private def parseConst(arg: String): Const =
    parseConstInt(arg)
      .orElse(parseConstIntWithLength(arg))
      .orElse(parseConstString(arg))
      .getOrElse(throw new IllegalArgumentException(s"can't parse const '$arg'"))

  private def parseConstInt(arg: String): Option[Const] =
    arg.toIntOption.map { value =>
      val buffer = Array.fill[Byte](4)(0)
      val bb = ByteBuffer.wrap(buffer)
      bb.order(ByteOrder.LITTLE_ENDIAN)
      bb.putInt(value)
      Const.Literal(buffer)
    }

  private def parseConstIntWithLength(arg: String): Option[Const] = {
    val es = arg.split("_")
    if (es.length == 2) {
      es(0).toLongOption.flatMap { value =>
        es(1).toIntOption.map { length =>
          assert(length <= 8)
          val buffer = Array.fill[Byte](8)(0)
          val bb = ByteBuffer.wrap(buffer)
          bb.order(ByteOrder.LITTLE_ENDIAN)
          bb.putLong(value)
          val bytes = Array.fill[Byte](length)(0)
          System.arraycopy(buffer, 0, bytes, 0, length)
          Const.Literal(bytes)
        }
      }
    } else None
  }

  private def parseConstString(arg: String): Option[Const] =
    if (arg.startsWith("'") && arg.endsWith("'")) {
      // todo: remove this after I cover other cases
      val bytes = new String(arg.substring(1, arg.length - 1)).getBytes(Charset.forName("UTF-8"))
      Some(Const.Literal(bytes))
    } else None

  private def parseAddress(arg: String): Address =
    if (arg.startsWith("*#")) {
      Address.IndirectHeap(parseConst(arg.substring(2)))
    } else if (arg.startsWith("*")) {
      Address.DirectHeap(parseConst(arg.substring(1)))
    } else if (arg.startsWith("##")) {
      ???
    } else if (arg.startsWith("#")) {
      Address.DirectStack(parseConst(arg.substring(1)))
    } else {
      throw new IllegalArgumentException(s"can't parse address '$arg'")
    }

  private def parseConstOrAddress(arg: String): Const | Address =
    if (arg.startsWith("*") || arg.startsWith("#")) parseAddress(arg) else parseConst(arg)
}
