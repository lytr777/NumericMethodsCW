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

        for (t <- 350 until 650 by 10) {
            val T = t + 273.15
            val ans = System1.solve(T)

            println(ans)

            val G_H2 = D.D_H2(T) * (System1.DP_G.P_H2 - ans.P_H2) / Constants.R / T / 0.01
            val G_HCl = D.D_HCl(T) * (System1.DP_G.P_HCl - ans.P_HCl) / Constants.R / T / 0.01
            val G_AlCl = D.D_AlCl(T) * (System1.DP_G.P_AlCl - ans.P_AlCl) / Constants.R / T / 0.01
            val G_AlCl2 = D.D_AlCl2(T) * (System1.DP_G.P_AlCl2 - ans.P_AlCl2) / Constants.R / T / 0.01
            val G_AlCl3 = D.D_AlCl3(T) * (System1.DP_G.P_AlCl3 - ans.P_AlCl3) / Constants.R / T / 0.01


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

            a += ((1 / T, Math.log(Math.abs(G_AlCl))))
            b += ((1 / T, Math.log(Math.abs(G_AlCl2))))
            c += ((1 / T, Math.log(Math.abs(G_AlCl3))))
            d += ((1 / T, Math.log(Math.abs(V_AL))))

            println()
        }

        val chart = XYLineChart(b)

        dataset.addSeries(b.toXYSeries("G_AlCl"))
        dataset.addSeries(a.toXYSeries("G_AlCl2"))
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

            val G_H2 = D.D_H2(T) * (System2.DP_G.P_H2 - ans.P_H2) / Constants.R / T / 0.01
            val G_HCl = D.D_HCl(T) * (System2.DP_G.P_HCl - ans.P_HCl) / Constants.R / T / 0.01
            val G_GaCl = D.D_GaCl(T) * (System2.DP_G.P_GaCl - ans.P_GaCl) / Constants.R / T / 0.01
            val G_GaCl2 = D.D_GaCl2(T) * (System2.DP_G.P_GaCl2 - ans.P_GaCl2) / Constants.R / T / 0.01
            val G_GaCl3 = D.D_GaCl3(T) * (System2.DP_G.P_GaCl3 - ans.P_GaCl3) / Constants.R / T / 0.01


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
        var a: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var b: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var c: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()
        var d: mutable.ArrayBuffer[(Double, Double)] = mutable.ArrayBuffer()

        val dataset = new XYSeriesCollection()

        val T = 1100 + 273.15
        for (x_g <- 0.0 until 1.0 by 0.1) {
            println("x_g: " + x_g)

            def it(h2_p: Double, d: mutable.ArrayBuffer[(Double, Double)]) = {
                val ans = System3.solve(h2_p, x_g)
                println(ans)

                val G_AlCl3 = D.D_AlCl3(T) * (System3.DP_G(h2_p, ans.x).P_AlCl3 - ans.P_AlCl3) / Constants.R / T / 0.01
                println("G_AlCl3: " + G_AlCl3)

                val G_GaCl = D.D_GaCl(T) * (System3.DP_G(h2_p, ans.x).P_GaCl - ans.P_GaCl) / Constants.R / T / 0.01
                println("G_GaCl: " + G_GaCl)

                val V_AlGaN = (G_AlCl3 * Elements.AlN.mu / 3200 + G_GaCl * Elements.GaN.mu / 6150) * Math.pow(10, 9)


                println("V_AlGaN: " + V_AlGaN)
                d += ((x_g, ans.x))
            }

            println("H2 p: 0")
            it(0, a)
            println()
            println("H2 p: 0.1")
            it(0.1, b)


            println()
        }

        val chart = XYLineChart(a)

        dataset.addSeries(a.toXYSeries("V_AlGaN at H2 = 0"))
        dataset.addSeries(b.toXYSeries("V_AlGaN at H2 = 0.1"))
//        dataset.addSeries(b.toXYSeries("G_GaCl2"))
//        dataset.addSeries(c.toXYSeries("G_GaCl3"))
//        dataset.addSeries(d.toXYSeries("V_Ga"))

        chart.plot.setDataset(dataset)
        chart.show()
    }

    override def main(args: Array[String]): Unit = {

        display3()

    }
}
