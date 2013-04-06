package com.sudoku.test

import org.scalatest.FunSuite
import com.sudoku.Table

class TableTest extends FunSuite {

  test("width") {
    expect(9) { new Table().width }
  }

  test("height") {
    expect(9) { new Table().height }
  }
}
