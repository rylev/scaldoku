package com.sudoku.test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.sudoku.Table

class TableTest extends FlatSpec with ShouldMatchers {

  "A defaul Table" should "have a width of 9" in {
    new Table().width should be(9)
  }

  it should "have a height of 9" in {
    new Table().height should be(9)
  }

  it can "set and get the Values" in {
    val table = (new Table).fillCell(column = 5, row = 6, value = 5)
    table.getCell(column = 5, row = 6).value should be (5)
  }

  it can "validate complete columns" in {
    val table = (0 to 8).foldLeft(new Table) { (sum, num) =>
      sum.fillCell(column = 0, row = num, value = num + 1) }

    table.completeColumn_?(0) should be(true)
  }

  it can "validate incomplete columns" in {
    val table = (0 to 8).foldLeft(new Table) { (sum, num) =>
      sum.fillCell(column = 0, row = num, value = 5) }

    table.completeColumn_?(0) should be(false)
  }

  it can "validate valid columns" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = 0, row = num, value = num + 1) }

    table.fillCell(column = 0, row = 3, value = 0).validColumn_?(0) should be(true)
  }

  it can "validate invalid columns" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = 0, row = num, value = num + 1) }

    table.fillCell(column = 0, row = 2, value = 9).fillCell(column = 0, row = 7, value = 4).validColumn_?(0) should be(false)
  }

  it can "validate complete rows" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = num, row = 0, value = num + 1) }

    table.completeRow_?(0) should be(true)
  }

  it can "validate incomplete rows" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = num, row = 0, value = 5) }

    table.completeRow_?(0) should be(false)
  }

  it can "validate valid rows" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = num, row = 0, value = num + 1) }

    table.fillCell(column = 3, row = 0, value = 0).validRow_?(0) should be(true)
  }

  it can "validate invalid rows" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = num, row = 0, value = num + 1) }

    table.fillCell(column = 2, row = 0, value = 9).fillCell(column = 8, row = 0, value = 4).validRow_?(0) should be(false)
  }

  it can "validate valid squares" in {
    val table = new Table().
      fillCell(column = 0, row = 0, value = 1).
      fillCell(column = 1, row = 0, value = 2).
      fillCell(column = 2, row = 0, value = 3).
      fillCell(column = 0, row = 1, value = 4).
      fillCell(column = 1, row = 1, value = 5).
      fillCell(column = 2, row = 1, value = 6)

    table.validSquare_?(0) should be(true)
}

  it can "validate invalid squares" in {
    val table = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.fillCell(column = num, row = 0, value = num + 1).
          fillCell(column = num, row = 1, value = num + 1) }

    table.validSquare_?(0) should be(false)
  }
}
