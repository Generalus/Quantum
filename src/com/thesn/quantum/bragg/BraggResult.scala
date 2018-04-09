package com.thesn.quantum.bragg

import org.knowm.xchart.{QuickChart, SwingWrapper, XYChart}

case class BraggResult(coordinates: Array[Double], result: Array[Complex], n: Array[Double], T: Double) {

  def showRealChart(): Unit = {
    val chart = QuickChart.getChart("Real chart", "Z", "ReY", "y(z)", coordinates, result.map(_.re))
    //chart.getStyler.setYAxisLogarithmic(true)
    new SwingWrapper[XYChart](chart).displayChart
  }

  def showModuleChart(): Unit = {
    val chart = QuickChart.getChart("Module chart", "Z", "|Y|", "y(z)", coordinates, result.map(!_))
    //chart.getStyler.setYAxisLogarithmic(true)
    new SwingWrapper[XYChart](chart).displayChart
  }

  def showNChart(): Unit = {
    val chart = QuickChart.getChart("n chart", "Z", "n", "n(z)", coordinates, n)
    //chart.getStyler.setYAxisLogarithmic(true)
    new SwingWrapper[XYChart](chart).displayChart
  }

}
