package ru.nobird.nm.cw.algebra

import ru.nobird.nm.cw.{D, K}

/**
  * Created by lytr777 on 31/05/2017.
  */
object System3 {
    case class Pressures(P_AlCl3: Double, P_GaCl: Double, P_NH3: Double, P_HCl: Double, P_H2: Double, x: Double) {
        def -(that: Pressures): Pressures
        = Pressures(P_AlCl3 - that.P_AlCl3, P_GaCl - that.P_GaCl, P_NH3 - that.P_NH3, P_HCl - that.P_HCl, P_H2 - that.P_H2, x - that.x)

        def toArray: Array[Double] = Array(P_AlCl3, P_GaCl, P_NH3, P_HCl, P_H2, x)
    }

    def DP_G(H2_part: Double, xg: Double): Pressures = {
        Pressures(30 * xg, 30 * (1 - xg), 1500, 0, 98470 * H2_part, 0)
    }

    private def preF(T: Double, P_G: Pressures) = (P_E: Pressures) =>
        Array(
            P_E.P_AlCl3 * P_E.P_NH3 - K.K9(T) * P_E.x * Math.pow(P_E.P_HCl, 3),
            P_E.P_GaCl * P_E.P_NH3 - K.K10(T) * (1 - P_E.x) * P_E.P_HCl * P_E.P_H2,

            D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl) + 2 * D.D_H2(T) * (P_G.P_H2 - P_E.P_H2)
                + 3 * D.D_NH3(T) * (P_G.P_NH3 - P_E.P_NH3),

            3 * D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) + D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
                + D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl),

            D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) + D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
                - D.D_NH3(T) * (P_G.P_NH3 - P_E.P_NH3),

            (1 - P_E.x) * D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) - P_E.x * D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
        )

    private def preJ(T: Double, P_G: Pressures) = (P_E: Pressures) =>
        Array(
            Array(P_E.P_NH3, 0, 0, -3 * D.D_AlCl3(T), -D.D_AlCl3(T), (P_E.x - 1) * D.D_AlCl3(T)),
            Array(0, P_E.P_NH3, 0, -D.D_GaCl(T), -D.D_GaCl(T), P_E.x * D.D_GaCl(T)),
            Array(P_E.P_AlCl3, P_E.P_GaCl, -3 * D.D_NH3(T), 0, D.D_NH3(T), 0),
            Array(-3 * K.K9(T) * P_E.x * Math.pow(P_E.P_HCl, 2), -K.K10(T) * (1 - P_E.x) * P_E.P_H2, -D.D_HCl(T), -D.D_HCl(T), 0, 0),
            Array(0, -K.K10(T) * (1 - P_E.x) * P_E.P_HCl, -2 * D.D_H2(T), 0, 0, 0),
            Array(-K.K9(T) * Math.pow(P_E.P_HCl, 3), K.K10(T) * P_E.P_HCl * P_E.P_H2, 0, 0, 0,
            -D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) - D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl))
        )

    def solve(H2_part: Double, xg: Double): Pressures = {
        val T = 1100 + 273.15
        val f = preF(T, DP_G(H2_part, xg))
        val J = preJ(T, DP_G(H2_part, xg))

        def iterate(x0: Pressures): Pressures = {
            val f_c = f(x0)
            val J_T = J(x0).transpose

            val x_a = Matrix.multiply(Matrix.inverse(J_T), f_c)

            x0 - Pressures(x_a(0), x_a(1), x_a(2), x_a(3), x_a(4), x_a(5))
        }

        var x = Pressures(100, 1, 10000, 1, 1, 0.5)
        var y = iterate(x)

        while ((x - y).toArray.map{ Math.abs }.max > Matrix.EPS) {
            x = y
            y = iterate(y)
        }

        y
    }
}
