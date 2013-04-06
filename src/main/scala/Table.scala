package com.sudoku

class Table {
  // TODO: at some point we should define something smarter than 0 for empty.
  var board = 1 to 9 map { _ => 1 to 9 map { _ => 0 } }

  def height = board.first.length
  def width = board.length

  def setValue(x: Integer, y: Integer, value: Integer): Boolean ={
    (x, y, value) match {
      case (a, b, c) if (0 to 8 contains a) &&
                        (0 to 8 contains b) &&
                        (0 to 9 contains c) => true
      case _ => return false
    }

    board = board.indices map { col_idx =>
      board(col_idx).indices map { row_idx =>
        if (col_idx == x && row_idx == y)
          value.intValue()
        else
          board(col_idx)(row_idx)
      }
    }
    true
  }

  def getValue(x: Integer, y: Integer) : Integer = {
    board(x)(y)
  }

  def completeRow_?(rowNumber: Integer) : Boolean = {
    val rowSet = row(rowNumber).toSet
    rowSet.size == 9 && rowSet.min == 1 && rowSet.max == 9
  }

  def validRow_?(rowNumber: Integer) : Boolean = {
    val cleanRow = row(rowNumber).filter { value => value != 0 }
    cleanRow.toSet.size == cleanRow.size
  }

  def row(number: Integer) = board.map { column => column(number) }
}