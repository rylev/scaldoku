package com.sudoku

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

class Square(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = false
  def valid_? : Boolean = {
    val cleanSquareCells = square.filter { !_.empty_? }
    cleanSquareCells.distinct.size == cleanSquareCells.size
  }

  private def square : Seq[Cell] = {
    val xOffset = (index / 3) * 3
    val yOffset = (index % 3) * 3

    (0 to 2 map { a =>
      0 to 2 map { b =>
        boardContents(xOffset+a)(yOffset+b)
      }
    }).flatten
  }
}

class Row(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueRow = row.distinct
    uniqueRow.size == 9 && uniqueRow.minBy { _.value }.value == 1 && uniqueRow.maxBy { _.value }.value == 9
  }

  def valid_? : Boolean = {
    val cleanRowCells = row.filter { !_.empty_? }
    cleanRowCells.distinct.size == cleanRowCells.size
  }

  override def toString : String =
    row.zip(row.indices).foldLeft("") { case (output, (cell, index)) =>
      output + cell.toString + (if ((index % 3) == 2) "  " else " ")
    }

  private def row: Seq[Cell] = boardContents.map { column => column(index) }
}

class Column(val boardContents: Seq[Seq[Cell]], val index: Integer) {
  def complete_? : Boolean = {
    val uniqueColumn = column.distinct
    uniqueColumn.size == 9 && (uniqueColumn.minBy { _.value }.value == 1) && (uniqueColumn.maxBy { _.value }.value == 9)
  }

  def valid_? : Boolean = {
    val cleanColumnCells = column.filter { !_.empty_? }
    cleanColumnCells.distinct.size == cleanColumnCells.size
  }

  private def column: Seq[Cell] = boardContents(index)
}

class Table(val contents: Seq[Seq[Cell]]) {
  def this() = this(1 to 9 map { _ => 1 to 9 map { _ => new Cell } })

  val height = contents(0).length
  val width = contents.length

  private val rows = 0 to 8 map { new Row(contents, _) }
  private val columns = 0 to 8 map { new Column(contents, _) }
  private val squares = 0 to 8 map { new Square(contents, _) }

  def fillCell(row: Integer, column: Integer, value: Integer): Table = {
    (row, column, value) match {
      case (row, column, value) if (0 to 8 contains row) &&
                                   (0 to 8 contains column) &&
                                   (0 to 9 contains value) => true
      case _ => return new Table(contents)
    }

    val newContents = contents.indices map { colIdx =>
      contents(colIdx).indices map { rowIdx =>
        if (colIdx == column && rowIdx == row) new Cell(value)
        else contents(colIdx)(rowIdx) } }

    new Table(newContents)
  }

  def getCell(row: Integer, column: Integer): Cell = {
    contents(column)(row)
  }

  def completeRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) complete_?

  def validRow_?(rowNumber: Integer) : Boolean = rows(rowNumber) valid_?

  def completeColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) complete_?

  def validColumn_?(columnNumber: Integer) : Boolean = columns(columnNumber) valid_?

  def completeSquare_?(squareNumber: Integer) : Boolean = squares(squareNumber) complete_?

  def validSquare_?(squareNumber: Integer) : Boolean = squares(squareNumber) valid_?

  override def toString : String =
    rows.zip(rows.indices).foldLeft("") { case (output, (row, index)) =>
      output + row.toString + (if ((index % 3) == 2) "\n\n" else "\n")
    }
}
