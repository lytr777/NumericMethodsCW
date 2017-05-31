package ru.nobird.nm.cw.algebra

import ru.nobird.nm.cw.{D, K}

/**
  * Created by ruslandavletshin on 31/05/2017.
  */
object System1 {
    case class Pressures(P_AlCl: Double, P_AlCl2: Double, P_AlCl3: Double, P_HCl: Double, P_H2: Double) {
        def -(that: Pressures): Pressures
            = Pressures(P_AlCl - that.P_AlCl, P_AlCl2 - that.P_AlCl2, P_AlCl3 - that.P_AlCl3, P_HCl - that.P_HCl, P_H2 - that.P_H2)

        def toArray: Array[Double] = Array(P_AlCl, P_AlCl2, P_AlCl3, P_HCl, P_H2)
    }

    lazy val DP_G = Pressures(0, 0, 0, 10000, 0)

    private def preF(T: Double, P_G: Pressures) = (P_E: Pressures) =>
        Array(
            Math.pow(P_E.P_HCl, 2) - K.K1(T) * Math.pow(P_E.P_AlCl, 2) * P_E.P_H2,
            Math.pow(P_E.P_HCl, 2) - K.K2(T) * P_E.P_AlCl2 * P_E.P_H2,
            Math.pow(P_E.P_HCl, 6) - K.K3(T) * Math.pow(P_E.P_AlCl3, 2) * Math.pow(P_E.P_H2, 3),

            D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl) + 2 * D.D_H2(T) * (P_G.P_H2 - P_E.P_H2),

            D.D_AlCl(T) * (P_G.P_AlCl - P_E.P_AlCl) + 2 * D.D_AlCl2(T) * (P_G.P_AlCl2 - P_E.P_AlCl2)
                + 3 * D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) + D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl)
        )

    private def preJ(T: Double) = (P_E: Pressures) =>
        Array(
            Array(-2 * K.K1(T) * P_E.P_AlCl * P_E.P_H2, 0, 0, 0, -D.D_AlCl(T)),
            Array(0, -K.K2(T) * P_E.P_H2, 0, 0, -2 * D.D_AlCl2(T)),
            Array(0, 0, -2 * K.K3(T) * P_E.P_AlCl3 * Math.pow(P_E.P_H2, 3), 0, -3 * D.D_AlCl3(T)),
            Array(2 * P_E.P_HCl, 2 * P_E.P_HCl, 6 * Math.pow(P_E.P_HCl, 5), -D.D_HCl(T), -D.D_HCl(T)),
            Array(-K.K1(T) * Math.pow(P_E.P_AlCl, 2), -K.K2(T) * P_E.P_AlCl2, -3 * K.K3(T) * Math.pow(P_E.P_AlCl3 * P_E.P_H2, 2), -2 * D.D_H2(T), 0)
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
