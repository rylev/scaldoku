package com.sudoku

class Table {
  var board = 1 to 9 map { _ => 1 to 9 map { _ => null } }

  def height = board.first.length
  def width = board.length
}