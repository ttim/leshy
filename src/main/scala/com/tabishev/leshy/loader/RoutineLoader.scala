package com.tabishev.leshy.loader

import com.tabishev.leshy.ast.Subroutine
import com.tabishev.leshy.parser.TextParser

import java.nio.file.{Files, Path}

trait RoutineLoader {
  def load(name: String): Option[Subroutine]
}

case class MapLoader(routines: Map[String, Subroutine]) extends RoutineLoader {
  override def load(name: String): Option[Subroutine] = routines.get(name)
}

object FileLoader {
  def fromFile(path: Path): RoutineLoader =
    MapLoader(TextParser.parse(Files.readString(path)))
}
