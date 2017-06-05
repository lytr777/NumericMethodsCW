package ru.nobird.nm.cw

import ru.nobird.nm.cw.algebra.{System1, System2, System3}

import scala.collection.mutable

/**
  * Created by ruslandavletshin on 30/05/2017.
  */
object Main extends App with scalax.chart.module.Charting  {

    private def X(T: Double) = T / Math.pow(10, 4)

    private def preparePhi(e: Element) = (X: Double) =>
        e.f1 + e.f2 * Math.log(X) + e.f3 / (X * X) + e.f4 / X + e.f5 * X + e.f6 * (X * X) + e.f7 * Math.pow(X, 3)

    private def prepareG(e: Element) = (T: Double) =>
        e.H - preparePhi(e)(X(T)) * T


    private def display1() = {
        var a: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var b: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var c: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var d: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dataset = new XYSeriesCollection()

        for (t <- 350 until 650 by 15) {
            val T = t + 273.15
            val ans = System1.solve(T)
            println("T: " + T)

            println(ans)

            val G_H2 = D.D_H2(T) * (System1.DP_G.P_H2 - ans.P_H2) / Constants.R / T / Constants.delta
            val G_HCl = D.D_HCl(T) * (System1.DP_G.P_HCl - ans.P_HCl) / Constants.R / T / Constants.delta
            val G_AlCl = D.D_AlCl(T) * (System1.DP_G.P_AlCl - ans.P_AlCl) / Constants.R / T / Constants.delta
            val G_AlCl2 = D.D_AlCl2(T) * (System1.DP_G.P_AlCl2 - ans.P_AlCl2) / Constants.R / T / Constants.delta
            val G_AlCl3 = D.D_AlCl3(T) * (System1.DP_G.P_AlCl3 - ans.P_AlCl3) / Constants.R / T / Constants.delta


            println("G_H2: " + G_H2)
            println("G_HCl: " + G_HCl)

            println("G_AlCl: " + G_AlCl)
            println("ln(G_AlCl): " + Math.log(Math.abs(G_AlCl)))

            println("G_AlCl2: " + G_AlCl2)
            println("ln(G_AlCl2): " + Math.log(Math.abs(G_AlCl2)))

            println("G_AlCl3: " + G_AlCl3)
            println("ln(G_AlCl3): " + Math.log(Math.abs(G_AlCl3)))

            val V_AL = (G_AlCl + G_AlCl2 + G_AlCl3) * Elements.Al.mu / 2690 * Math.pow(10, 9)
            println("V_AL: " + V_AL)
            println("ln(V_AL): " + Math.log(Math.abs(V_AL)))

            println()
            println("D_AlCl: " +  D.D_AlCl(T))
            println("G_AlCl2: " +  D.D_AlCl2(T))
            println("D_AlCl3: " +  D.D_AlCl3(T))

            a += ((1 / T, Math.log(Math.abs(G_AlCl))))
            b += ((1 / T, Math.log(Math.abs(G_AlCl2))))
            c += ((1 / T, Math.log(Math.abs(G_AlCl3))))
            d += ((1 / T, Math.log(Math.abs(V_AL))))

            println()
        }

        val chart = XYLineChart(b)

        dataset.addSeries(a.toXYSeries("G_AlCl"))
        dataset.addSeries(b.toXYSeries("G_AlCl2"))
        dataset.addSeries(c.toXYSeries("G_AlCl3"))
        dataset.addSeries(d.toXYSeries("V_AL"))

        chart.plot.setDataset(dataset)
        chart.show()
    }

    private def display2() = {
        var a: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var b: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var c: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var d: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dataset = new XYSeriesCollection()

        for (t <- 650 until 950 by 10) {
            val T = t + 273.15
            println("T: " + T)
            val ans = System2.solve(T)

            println(ans)

            val G_H2 = D.D_H2(T) * (System2.DP_G.P_H2 - ans.P_H2) / Constants.R / T / Constants.delta
            val G_HCl = D.D_HCl(T) * (System2.DP_G.P_HCl - ans.P_HCl) / Constants.R / T / Constants.delta
            val G_GaCl = D.D_GaCl(T) * (System2.DP_G.P_GaCl - ans.P_GaCl) / Constants.R / T / Constants.delta
            val G_GaCl2 = D.D_GaCl2(T) * (System2.DP_G.P_GaCl2 - ans.P_GaCl2) / Constants.R / T / Constants.delta
            val G_GaCl3 = D.D_GaCl3(T) * (System2.DP_G.P_GaCl3 - ans.P_GaCl3) / Constants.R / T / Constants.delta


            println("G_H2: " + G_H2)
            println("G_HCl: " + G_HCl)

            println("G_GaCl: " + G_GaCl)
            println("ln(G_GaCl): " + Math.log(Math.abs(G_GaCl)))

            println("G_GaCl2: " + G_GaCl2)
            println("ln(G_GaCl2): " + Math.log(Math.abs(G_GaCl2)))

            println("G_GaCl3: " + G_GaCl3)
            println("ln(G_GaCl3): " + Math.log(Math.abs(G_GaCl3)))

            val V_Ga = (G_GaCl + G_GaCl2 + G_GaCl3) * Elements.Ga.mu / 5900 * Math.pow(10, 9)
            println("V_Ga: " + V_Ga)
            println("ln(V_Ga): " + Math.log(Math.abs(V_Ga)))

            a += ((1 / T, Math.log(Math.abs(G_GaCl))))
            b += ((1 / T, Math.log(Math.abs(G_GaCl2))))
            c += ((1 / T, Math.log(Math.abs(G_GaCl3))))
            d += ((1 / T, Math.log(Math.abs(V_Ga))))

            println()
        }

        val chart = XYLineChart(b)

        dataset.addSeries(a.toXYSeries("G_GaCl"))
        dataset.addSeries(b.toXYSeries("G_GaCl2"))
        dataset.addSeries(c.toXYSeries("G_GaCl3"))
        dataset.addSeries(d.toXYSeries("V_Ga"))

        chart.plot.setDataset(dataset)
        chart.show()
    }


    private def display3() = {
        val a: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        val b: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dsAlCl3x0: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        val dsAlCl3x1: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dsGaClx0: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        val dsGaClx1: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dsVAlGaNx0: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        val dsVAlGaNx1: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dataset = new XYSeriesCollection()

        val T = 1100 + 273.15

        def it(h2_p: Double, x_g: Double) = {
            val ans = System3.solve(h2_p, x_g)
            println(ans)

            val G_AlCl3 = D.D_AlCl3(T) * (System3.DP_G(h2_p, ans.x).P_AlCl3 - ans.P_AlCl3) / Constants.R / T / Constants.delta
            println("G_AlCl3: " + G_AlCl3)

            val G_GaCl = D.D_GaCl(T) * (System3.DP_G(h2_p, ans.x).P_GaCl - ans.P_GaCl) / Constants.R / T / Constants.delta
            println("G_GaCl: " + G_GaCl)

            val V_AlGaN = (G_AlCl3 * Elements.AlN.mu / 3200 + G_GaCl * Elements.GaN.mu / 6150) * Math.pow(10, 9)


            println("V_AlGaN: " + V_AlGaN)

            if (h2_p == 0.0) {
                a += ((x_g, ans.x))
                dsAlCl3x0 += ((x_g, G_AlCl3))
                dsGaClx0 += ((x_g, G_GaCl))
                dsVAlGaNx0 += ((x_g, V_AlGaN))
            } else {
                b += ((x_g, ans.x))
                dsAlCl3x1 += ((x_g, G_AlCl3))
                dsGaClx1 += ((x_g, G_GaCl))
                dsVAlGaNx1 += ((x_g, V_AlGaN))
            }
        }

        for (x_g <- 0.0 until 1.0 by 0.05) {
            println("x_g: " + x_g)
            println("H2 p: 0")
            it(0, x_g)
            println()
            println("H2 p: 0.1")
            it(0.1, x_g)
            println()
        }

        val chart = XYLineChart(dsAlCl3x0)
//
//        dataset.addSeries(a.toXYSeries("x at H2 = 0"))
//        dataset.addSeries(b.toXYSeries("x at H2 = 0.1"))

//        dataset.addSeries(dsAlCl3x0.toXYSeries("G_AlCl3 at H2 = 0"))
//        dataset.addSeries(dsAlCl3x1.toXYSeries("G_AlCl3 at H2 = 0.1"))
//
//        dataset.addSeries(dsGaClx0.toXYSeries("G_GaCl at H2 = 0"))
//        dataset.addSeries(dsGaClx1.toXYSeries("G_GaCl at H2 = 0.1"))

        dataset.addSeries(dsVAlGaNx0.toXYSeries("V_AlGaN at H2 = 0"))
        dataset.addSeries(dsVAlGaNx1.toXYSeries("V_AlGaN at H2 = 0.1"))


        chart.plot.setDataset(dataset)
        chart.show()
    }

    override def main(args: Array[String]): Unit = {

        display3()

    }
}
