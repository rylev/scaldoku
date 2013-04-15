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
    val table = new Table
    val newTable = table.setValue(x = 5, y = 6, value = 5)
    newTable.getValue(x = 5, y = 6) should be (5)
  }

  it can "validate complete rows" in {
    val table = new Table
    val newTable = (0 to 8).foldLeft(table) { (sum, num) =>
      sum.setValue(x = num, y = 0, value = num + 1) }

    newTable.completeRow_?(0) should be(true)
  }

  it can "validate incomplete rows" in {
    val table = new Table
    val newTable = (0 to 8).foldLeft(table) { (sum, num) =>
      sum.setValue(x = num, y = 0, value = 5) }

    newTable.completeRow_?(0) should be(false)
  }

  it can "validate valid rows" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = num, y = 0, value = num + 1) }

    newTable.setValue(x = 3, y = 0, value = 0).validRow_?(0) should be(true)
  }

  it can "validate invalid rows" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = num, y = 0, value = num + 1) }

    newTable.setValue(x = 2, y = 0, value = 9).setValue(x = 7, y = 0, value = 4).validRow_?(0) should be(false)
  }

  it can "validate complete columns" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = 0, y = num, value = num + 1) }

    newTable.completeColumn_?(0) should be(true)
  }

  it can "validate incomplete columns" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = 0, y = num, value = 5) }

    newTable.completeColumn_?(0) should be(false)
  }

  it can "validate valid columns" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = 0, y = num, value = num + 1) }

    newTable.setValue(x = 0, y = 3, value = 0).validColumn_?(0) should be(true)
  }

  it can "validate invalid columns" in {
    val newTable = (0 to 8).foldLeft(new Table()) { (sum, num) =>
      sum.setValue(x = 0, y = num, value = num + 1) }

    newTable.setValue(x = 0, y = 2, value = 9).setValue(x = 0, y = 8, value = 4).validColumn_?(0) should be(false)
  }

}
