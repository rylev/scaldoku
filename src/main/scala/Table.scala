package com.sudoku

class Table {
  var board = 1 to 9 map { _ => 1 to 9 map { _ => -1 } }

  def height = board.first.length
  def width = board.length

  def setValue(x: Integer, y: Integer, value: Integer): Boolean ={
    (x, y, value) match {
      case (a, b, c) if (0 until 8 contains a) &&
                        (0 until 8 contains b) &&
                        (1 until 9 contains c) => true
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

  def getValue(x: Integer, y: Integer): Integer = {
    board(x)(y)
  }
}