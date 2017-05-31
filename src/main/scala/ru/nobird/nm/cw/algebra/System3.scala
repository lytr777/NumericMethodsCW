package ru.nobird.nm.cw.algebra

import ru.nobird.nm.cw.{D, K}

/**
  * Created by lytr777 on 31/05/2017.
  */
object System3 {
    case class Pressures(P_AlCl3: Double, P_GaCl: Double, P_NH3: Double, P_HCl: Double, P_H2: Double) {
        def -(that: Pressures): Pressures
        = Pressures(P_AlCl3 - that.P_AlCl3, P_GaCl - that.P_GaCl, P_NH3 - that.P_NH3, P_HCl - that.P_HCl, P_H2 - that.P_H2)

        def toArray: Array[Double] = Array(P_AlCl3, P_GaCl, P_NH3, P_HCl, P_H2)
    }

    private def DP_G(H2_part: Double, xg: Double): Pressures = {
        Pressures(30 * xg, 30 * (1 - xg), 1500, 98470 * (1 - H2_part), 98470 * H2_part)
    }

    private def preF(T: Double, P_G: Pressures) = (P_E: Pressures, x: Double) =>
        Array(
            P_E.P_AlCl3 * P_E.P_NH3 - K.K4(T) * x * Math.pow(P_E.P_HCl, 3),
            P_E.P_GaCl * P_E.P_NH3 - K.K10(T) * (1 - x) * P_E.P_HCl * P_E.P_H2,

            D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl) + 2 * D.D_H2(T) * (P_G.P_H2 - P_E.P_H2)
                + 3 * D.D_NH3(P_G.P_NH3 - P_E.P_NH3),

            3 * D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) + D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
                + D.D_HCl(T) * (P_G.P_HCl - P_E.P_HCl),

            D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) + D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
                - D.D_NH3(P_G.P_NH3 - P_E.P_NH3),

            (1 - x) * D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) - x * D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl)
        )

    private def preJ(T: Double, P_G: Pressures) = (P_E: Pressures, x: Double) =>
        Array(
            Array(P_E.P_NH3, 0, 0, -3 * D.D_AlCl3(T), -D.D_AlCl3(T), (x - 1) * D.D_AlCl3(T)),
            Array(0, P_E.P_NH3, 0, -D.D_GaCl(T), -D.D_GaCl(T), x * D.D_GaCl(T)),
            Array(P_E.P_AlCl3, P_E.P_GaCl, -3 * D.D_NH3(T), 0, D.D_NH3(T), 0),
            Array(-3 * K.K9(T) * x * Math.pow(P_E.P_HCl, 2), -K.K10(T) * (1 - x) * P_E.P_H2, -D.D_HCl(T), -D.D_HCl(T), 0, 0),
            Array(0, -K.K10(T) * (1 - x) * P_E.P_HCl, -2 * D.D_H2(T), 0, 0, 0),
            Array(-K.K9(T) * Math.pow(P_E.P_HCl, 3), K.K10(T) * P_E.P_HCl * P_E.P_H2, 0, 0, 0,
            -D.D_AlCl3(T) * (P_G.P_AlCl3 - P_E.P_AlCl3) - D.D_GaCl(T) * (P_G.P_GaCl - P_E.P_GaCl))
        )
}
