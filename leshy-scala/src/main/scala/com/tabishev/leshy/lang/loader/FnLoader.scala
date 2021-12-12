package com.tabishev.leshy.lang.loader

import com.tabishev.leshy.lang.ast.Fn
import com.tabishev.leshy.lang.parser.TextParser

import java.nio.file.{Files, Path}

trait FnLoader {
  def load(name: String): Option[Fn]
}

case class MapLoader(fns: Map[String, Fn]) extends FnLoader {
  override def load(name: String): Option[Fn] = fns.get(name)
}

object FileLoader {
  def fromFiles(paths: Seq[Path]): FnLoader =
    MapLoader(paths.flatMap(path => TextParser.parse(path, Files.readString(path)).toSeq).toMap)

  def fromFile(path: Path): FnLoader =
    MapLoader(TextParser.parse(path, Files.readString(path)))
}
