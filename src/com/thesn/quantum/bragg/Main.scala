package com.thesn.quantum.bragg


object Main {


  // TODO:
  // а построение графика по массивам вынести в ChartGenerator
  // makeArray(from: Double, to: Double, n: Int) вынести в отдельный утилитарный объект и пользоваться

  //https://knowm.org/open-source/xchart/xchart-example-code/
  def main(args: Array[String]): Unit = {
    val result = BraggProblem().solve()

    result.showModuleChart()
    result.showNChart()
    result.showRealChart()

  }


}