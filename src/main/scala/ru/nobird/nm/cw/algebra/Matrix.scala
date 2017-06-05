package ru.nobird.nm.cw.algebra

/**
  * Created by ruslandavletshin on 31/05/2017.
  */

object Matrix {
    def EPS = 0.001

    def E(s: Int): Array[Array[Double]] = {
        val m = Array.fill(s){Array.fill(s){0.0}}
        for (i <- m.indices) {
            m(i)(i) = 1
        }
        m
    }

    private def swapRows(m: Array[Array[Double]], a: Int, b: Int) = {
        val tmp = m(a)
        m(a) = m(b)
        m(b) = tmp
    }

    def printMatrix(m: Array[Array[Double]]) : Unit = {
        m.foreach(x => {
            x.foreach(y => print(y + " "))
            println()
        })
    }

    def inverse(m: Array[Array[Double]]): Array[Array[Double]] = {
        val dg = m.map(_.clone())
        val inv = E(m.length)

        for (k <- m.indices) {
            var min = m(k)(k)
            var row = k

            for (i <- k until m.length) {
                if (dg(i)(k) != 0) {
                    if (min == 0 || Math.abs(dg(i)(k)) < Math.abs(min)) {
                        min = dg(i)(k)
                        row = i
                    }
                }
            }


            if (k != row) {
                swapRows(dg, k, row)
                swapRows(inv, k, row)
            }

            if (min != 0) {
                for (i <- m.indices) {
                    if (i != k) {
                        val mul = dg(i)(k) / dg(k)(k)

                        for (j <- m.indices) {
                            dg(i)(j) -= mul * dg(k)(j)
                            inv(i)(j) -= mul * inv(k)(j)
                        }
                    }
                }
            }
        }

        for (i <- m.indices) {
            if (dg(i)(i) != 0) {
                for (j <- m.indices) {
                    inv(i)(j) /= dg(i)(i)
                }
            }
        }

        inv
    }

    def multiply(a: Array[Array[Double]], b: Array[Double]): Array[Double] = {
        val res = Array.fill(a.length){0.0}
        for (i <- a.indices) {
            for (j <- a(i).indices) {
                res(i) += a(i)(j) * b(j)
            }
        }
        res
    }

    def mult(a: Array[Array[Double]], b: Array[Array[Double]]):Array[Array[Double]] = {
        for (row <- a)
            yield for(col <- b.transpose)
                yield (row zip col map Function.tupled(_ * _)).sum
    }

}
