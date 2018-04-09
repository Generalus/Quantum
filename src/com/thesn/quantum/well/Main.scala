package com.thesn.quantum.well

import org.knowm.xchart.{QuickChart, SwingWrapper, XYChart}

object Main {
  def main(args: Array[String]): Unit = {
    val a = 2e-8 // 200 ангстрем
    val V = 1.60218e-19 // 1 ЭВ
    val m = 9.11e-31 // масса электрона
    val mx = 0.067 * m // эффективная масса электрона
    val h = 1.054571800e-34 // постоянная планка / 2PI


    def k2(E: Double) = Math.sqrt(2 * m * E) / h

    def k1(E: Double) = Math.sqrt(2 * m * (V - E)) / h

    def er(n: Int) = h * h / (2 * mx) * Math.pow(Math.PI * n / a, 2)

    def t(E: Double) = 1 / (1 + Math.pow(V * Math.sin(k2(E) * a), 2) / (4 * E * (E - V)))

    def makeArray(from: Double, to: Double, n: Int) = {
      val dz = (to - from) / (n - 1)
      Iterator.from(1).map(from + _ * dz - dz).take(n).toArray
    }

    val e = makeArray(V, 2 * V, 10000)



    val tResultArray = e.map(t)
    showTChart(e.map(_*1e20), tResultArray.map(_*1e20))



    println("Энергия первого резонансного уровня: " + er(1))

    def psi2(x: Double) = if (x >= 0 && x <= a) Math.pow(Math.sin(k1(V + er(1))*x), 2) else
    if (x > a) Math.pow(Math.exp(-k1(er(1))*x), 2) else ???

    val xs = makeArray(0.2*a, 1.2 * a, 10000)

    showPChart(xs, xs.map(psi2))


  }


  def showTChart(z: Array[Double], y: Array[Double]): Unit = {
    val chart = QuickChart.getChart("Зависимость коэффициента прохождения от энергии", "E*e20", "T*e20", "t(e)", z, y)
    //chart.getStyler.setYAxisLogarithmic(true)
    new SwingWrapper[XYChart](chart).displayChart
  }

  def showPChart(z: Array[Double], y: Array[Double]): Unit = {
    val chart = QuickChart.getChart("Зависимость волновой функции от ширины координаты при энергии соотв. первому резонансному уровню", "x", "psi2", "psi2(x)", z, y)
    //chart.getStyler.setYAxisLogarithmic(true)
    new SwingWrapper[XYChart](chart).displayChart
  }


}
