package ru.nobird.nm.cw

/**
  * Created by ruslandavletshin on 30/05/2017.
  */
object K {

    private def X(T: Double) = T / Math.pow(10, 4)

    private def preparePhi(e: Element) = (X: Double) =>
        e.f1 + e.f2 * Math.log(X) + e.f3 / (X * X) + e.f4 / X + e.f5 * X + e.f6 * (X * X) + e.f7 * Math.pow(X, 3)

    private def prepareG(e: Element) = (T: Double) =>
        e.H - preparePhi(e)(X(T)) * T



    def dG1(T: Double): Double = 2 * prepareG(Elements.Al)(T) + 2 * prepareG(Elements.HCl)(T) -
        2 * prepareG(Elements.AlCl)(T) - prepareG(Elements.H2)(T)

    def dG2(T: Double): Double = prepareG(Elements.Al)(T) + 2 * prepareG(Elements.HCl)(T) -
        prepareG(Elements.AlCl2)(T) - prepareG(Elements.H2)(T)

    def dG3(T: Double): Double = 2 * prepareG(Elements.Al)(T) + 6 * prepareG(Elements.HCl)(T) -
        2 * prepareG(Elements.AlCl3)(T) - 3 * prepareG(Elements.H2)(T)




    private def prepareK(dG: Double => Double) = (T: Double) =>
        Math.exp(-dG(T)/(Constants.R * T)) / Constants.Pa


    def K1(T: Double): Double = prepareK(dG1)(T)
    def K2(T: Double): Double = prepareK(dG2)(T)
    def K3(T: Double): Double = prepareK(dG3)(T)
}
