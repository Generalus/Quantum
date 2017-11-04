package com.thesn.quantum

object Main {


  def main(args: Array[String]): Unit = {

    val AII = 1

    val lambda = 1.2e-5 // ???

    val k = 2 * Math.PI / lambda


    // коэффициенты преломления

    val n1 = 3.4

    val nc = 3.4 // GaAs

    val n2 = 2.95 // AlAs

    val n0 = 1 // ???


    // размеры образца

    val d1 = 1 // ???

    val d2 = 1 // ???

    val dc = 1 // ???

    val h = 2 * d2 + dc // 0 < z < h


    val N = 100 // число шагов

    val dz = h * 1.0 / (N - 1)


    def n(z: Double): Double = if (z >= 0 && z <= d2) n2 else if (z >= d2 && z <= d2 + dc) nc else if (z >= d2 + dc && z <= h) n1 else n0


    val z: Array[Double] = Iterator.from(1).map(_ * dz - dz).take(N).toArray


    val d = k * n0 * dz // знаменатель


    val A = Array(Complex(0, 0)) ++ Array.fill(N - 2)(1.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(0, -1.0 / d))

    val C = Array(Complex(1, 1.0 / d)) ++ z.slice(1, z.length - 1).map(z => Math.pow(k * n(z), 2) - 2.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(1, 1.0 / d))

    val B = Array(Complex(0, -1.0 / d)) ++ Array.fill(N - 2)(1.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(0, 0))

    val F = Array(Complex(AII, 0)) ++ Array.fill(N - 1)(Complex(0, 0))

    val alpha = Array.fill(N)(Complex(0, 0))
    val beta = Array.fill(N)(Complex(0, 0))


    alpha(1) = -B(0) / C(0)
    beta(1) = F(0) / C(0)

    for (i <- 2 until N) {
      val j = i - 1
      alpha(i) = -B(j) / (A(j) * alpha(j) + C(j))
      beta(i) = (F(j) - A(j) * beta(j)) / (A(j) * alpha(j) + C(i))
    }

    val x = Array.fill(N)(Complex(0, 0))

    x(N - 1) = (F(N - 1) - A(N - 1) * beta(N - 1)) / (C(N - 1) + A(N - 1) * alpha(N - 1))

    for (i <- N - 2 to(0, -1)) {
      x(i) = alpha(i + 1) * x(i + 1) + beta(i + 1)
    }

    x.foreach(println)

  }


}