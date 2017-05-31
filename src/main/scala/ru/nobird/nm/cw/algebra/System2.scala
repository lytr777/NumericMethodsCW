package ru.nobird.nm.cw.algebra

import ru.nobird.nm.cw.{D, K}

/**
  * Created by lytr777 on 31/05/2017.
  */
object System2 {
    case class Pressures(P_GaCl: Double, P_GaCl2: Double, P_GaCl3: Double, P_HCl: Double, P_H2: Double) {
        def -(that: Pressures): Pressures
        = Pressures(P_GaCl - that.P_GaCl, P_GaCl2 - that.P_GaCl2, P_GaCl3 - that.P_GaCl3, P_HCl - that.P_HCl, P_H2 - that.P_H2)

        def toArray: Array[Double] = Array(P_GaCl, P_GaCl2, P_GaCl3, P_HCl, P_H2)
    }

    lazy val DP_G = Pressures(0, 0, 0, 10000, 0)

    private def preF(T: Double, P_G: Pressures) = (P_E: Pressures) =>
        Array(
            Math.pow(P_E.P_HCl, 2) - K.K4(T) * Math.pow(P_E.P_GaCl, 2) * P_E.P_H2,
            Math.pow(P_E.P_HCl, 2) - K.K5(T) * P_E.P_GaCl2 * P_E.P_H2,
            Math.pow(P_E.P_HCl, 6) - K.K6(T) * Math.pow(P_E.P_GaCl3, 2) * Math.pow(P_E.P_H2, 3),

            D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl) + 2 * D.D_H2(T) * (P_G.P_H2 - P_E.P_H2),

            D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl) + 2 * D.D_GaCl2(T) * (P_G.P_GaCl2 - P_E.P_GaCl2)
                + 3 * D.D_GaCl3(T) * (P_G.P_GaCl3 - P_E.P_GaCl3) + D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl)
        )

    private def preJ(T: Double) = (P_E: Pressures) =>
        Array(
            Array(-2 * K.K4(T) * P_E.P_GaCl * P_E.P_H2, 0, 0, 0, -D.D_GaCl(T)),
            Array(0, -K.K5(T) * P_E.P_H2, 0, 0, -2 * D.D_GaCl2(T)),
            Array(0, 0, -2 * K.K6(T) * P_E.P_GaCl3 * Math.pow(P_E.P_H2, 3), 0, -3 * D.D_GaCl3(T)),
            Array(2 * P_E.P_HCl, 2 * P_E.P_HCl, 6 * Math.pow(P_E.P_HCl, 5), -D.D_HCl(T), -D.D_HCl(T)),
            Array(-K.K4(T) * Math.pow(P_E.P_GaCl, 2), -K.K5(T) * P_E.P_GaCl2, -3 * K.K6(T) * Math.pow(P_E.P_GaCl3 * P_E.P_H2, 2), -2 * D.D_H2(T), 0)
        )

    def solve(T: Double): Pressures = {
        val f = preF(T, DP_G)
        val J = preJ(T)

        def iterate(x0: Pressures): Pressures = {
            val f_c = f(x0)
            val J_T = J(x0).transpose

            val x_a = Matrix.multiply(Matrix.inverse(J_T), f_c)

            x0 - Pressures(x_a(0), x_a(1), x_a(2), x_a(3), x_a(4))
        }

        var x = Pressures(1, 1, 1, 1, 1)
        var y = iterate(x)

        while ((x - y).toArray.map{ Math.abs }.max > Matrix.EPS) {
            x = y
            y = iterate(y)
        }

        y
    }
}
