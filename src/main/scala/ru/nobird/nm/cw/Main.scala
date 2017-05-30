package ru.nobird.nm.cr

/**
  * Created by ruslandavletshin on 30/05/2017.
  */
object Main {

    private def X(T: Double) = T / Math.pow(10, 4)

    private def preparePhi(e: Element) = (X: Double) =>
        e.f1 + e.f2 * Math.log(X) + e.f3 / (X * X) + e.f4 / X + e.f5 * X + e.f6 * (X * X) + e.f7 * Math.pow(X, 3)

    private def prepareG(e: Element) = (T: Double) =>
        e.H - preparePhi(e)(X(T)) * T






    def main(args: Array[String]): Unit = {


    }
}
