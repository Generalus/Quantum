package com.thesn.quantum.bragg

case class BraggProblem(
                         var AII: Double = 1,
                         var lambda: Double = 1,

                         // коэффициенты преломления
                         var n1: Double = 3.4, // GaAs
                         var nc: Double = 3.4,
                         var n2: Double = 2.95, //AlAs
                         var n0: Double = 1,

                         // размеры образца
                         var d1: Double = 1,
                         var d2: Double = 1,
                         var dc: Double = 1,
                         var N: Int = 1000 // число шагов
                       ) {

  def solve(): BraggResult ={

    val k = 2 * Math.PI / lambda

    val h = 2 * d2 + dc // 0 < z < h

    val dz = h * 1.0 / (N - 1)


    def n(z: Double): Double = if (z >= 0 && z <= d2) n2 else if (z >= d2 && z <= d2 + dc) nc else if (z >= d2 + dc && z <= h) n1 else n0


    val z: Array[Double] = Iterator.from(1).map(_ * dz - dz).take(N).toArray


    val d = k * n0 * dz // знаменатель


    val A = Array(Complex(0, 0)) ++ Array.fill(N - 2)(1.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(0, -1.0 / d))

    val C = Array(Complex(1, 1.0 / d)) ++ z.slice(1, z.length - 1).map(z => Math.pow(k * n(z), 2) - 2.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(1, 1.0 / d))

    val B = Array(Complex(0, -1.0 / d)) ++ Array.fill(N - 2)(1.0 / Math.pow(dz, 2)).map(Complex(_, 0)) ++ Array(Complex(0, 0))

    val F = Array(Complex(2 * AII, 0)) ++ Array.fill(N - 1)(Complex(0, 0))

    val alpha = Array.fill(N)(Complex(0, 0))
    val beta = Array.fill(N)(Complex(0, 0))


    alpha(1) = -B(0) / C(0)
    beta(1) = F(0) / C(0)

    for (i <- 2 until N) {
      val j = i - 1
      alpha(i) = -B(j) / (A(j) * alpha(j) + C(j))
      beta(i) = (F(j) - A(j) * beta(j)) / (A(j) * alpha(j) + C(j))
    }

    val x = Array.fill(N)(Complex(0, 0))

    x(N - 1) = (F(N - 1) - A(N - 1) * beta(N - 1)) / (C(N - 1) + A(N - 1) * alpha(N - 1))

    for (i <- (N - 2) to(0, -1)) {
      x(i) = alpha(i + 1) * x(i + 1) + beta(i + 1)
    }

    BraggResult(z, x, z.map(n), Math.pow(!x(1)/AII, 2))
  }

}
