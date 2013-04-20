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

trait Region {
  val boardContents: Seq[Seq[Cell]]
  val index: Integer
  def cells: Seq[Cell]

  def complete_? : Boolean = {
    val uniqueCells = cells.distinct
    uniqueCells.size == 9 && (uniqueCells.minBy { _.value }.value == 1) && (uniqueCells.maxBy { _.value }.value == 9)
  }

  def valid_? : Boolean = {
    val nonEmptyCells = cells.filter { !_.empty_? }
    nonEmptyCells.distinct.size == nonEmptyCells.size
  }
}

class Square(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  def cells : Seq[Cell] = {
    val rowOffset = (index / 3) * 3
    val columnOffset = (index % 3) * 3

    (0 to 2 map { a =>
      0 to 2 map { b =>
        boardContents(columnOffset+a)(rowOffset+b)
      }
    }).flatten
  }
}

class Row(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  override def toString : String =
    cells.zip(cells.indices).foldLeft("") { case (output, (cell, index)) =>
      output + cell.toString + (if ((index % 3) == 2) "  " else " ")
    }

  def cells: Seq[Cell] = boardContents.map { column => column(index) }
}

class Column(val boardContents: Seq[Seq[Cell]], val index: Integer) extends Region {
  def cells: Seq[Cell] = boardContents(index)
}

class Table(val contents: Seq[Seq[Cell]]) {
  def this() = this(1 to 9 map { _ => 1 to 9 map { _ => new Cell } })

  val height = contents(0).length
  val width = contents.length

  private val rows = 0 to 8 map { new Row(contents, _) }
  private val columns = 0 to 8 map { new Column(contents, _) }
  private val squares = 0 to 8 map { new Square(contents, _) }

  def valid_? : Boolean =
    rows.forall(_.valid_?) && columns.forall(_.valid_?) && squares.forall(_.valid_?)

  def complete_? : Boolean =
    rows.forall(_.complete_?) && columns.forall(_.complete_?) && squares.forall(_.complete_?)

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
