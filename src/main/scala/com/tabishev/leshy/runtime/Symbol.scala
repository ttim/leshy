package com.tabishev.leshy.runtime

import com.tabishev.leshy.ast.Bytes

import scala.collection.mutable

object CommonSymbols {
  def register(symbols: Symbols): Unit =
    Seq("eq", "neq", "m", "le").foreach(symbols.register)
}

case class Symbol(name: String, id: Int) {
  val asBytes: Bytes = Bytes.fromInt(id)
}

class Symbols {
  private val symbolsByName: mutable.Map[String, Symbol] = mutable.HashMap()
  private val symbolsById: mutable.Map[Int, Symbol] = mutable.HashMap()
  private var nextSymbol: Int = 1

  def register(name: String): Symbol =
    if (symbolsByName.contains(name)) symbolsByName(name) else {
      val symbol = Symbol(name, nextSymbol)
      nextSymbol += 1

      symbolsByName.put(name, symbol)
      symbolsById.put(symbol.id, symbol)

      symbol
    }

  def resolve(name: String): Symbol = symbolsByName(name)
  def resolveBytes(bytes: Bytes): Symbol = symbolsById(bytes.asInt)
}
