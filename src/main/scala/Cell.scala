package com.sudoku.cell

class Cell(val value: Integer) {
  def this() = this(null)
  override def toString = if (empty_?) "_" else value.toString
  override def equals(other: Any) : Boolean = {
    (value, other.asInstanceOf[Cell].value) match {
      case (null, null) => true
      case (null, _) => false
      case (_, null) => false
      case (a, b) => a == b
    }
  }
  override def hashCode() : Int = if (empty_?) 0 else value.hashCode
  def empty_? : Boolean = value == null
}
