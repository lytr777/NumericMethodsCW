package ru.nobird.nm.cw

/**
  * Created by ruslandavletshin on 30/05/2017.
  */
object K {

    private def X(T: Double) = T / 1e4

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


    def dG4(T: Double): Double = 2 * prepareG(Elements.Ga)(T) + 2 * prepareG(Elements.HCl)(T) -
        2 * prepareG(Elements.GaCl)(T) - prepareG(Elements.H2)(T)

    def dG5(T: Double): Double = prepareG(Elements.Ga)(T) + 2 * prepareG(Elements.HCl)(T) -
        prepareG(Elements.GaCl2)(T) - prepareG(Elements.H2)(T)

    def dG6(T: Double): Double = 2 * prepareG(Elements.Ga)(T) + 6 * prepareG(Elements.HCl)(T) -
        2 * prepareG(Elements.GaCl3)(T) - 3 * prepareG(Elements.H2)(T)


    def dG7(T: Double): Double = prepareG(Elements.AlCl)(T) + prepareG(Elements.NH3)(T) -
        prepareG(Elements.AlN)(T) - prepareG(Elements.HCl)(T) - prepareG(Elements.H2)(T)

    def dG8(T: Double): Double = 2 * prepareG(Elements.AlCl2)(T) + 2 * prepareG(Elements.NH3)(T) -
        2 * prepareG(Elements.AlN)(T) - 4 * prepareG(Elements.HCl)(T) - prepareG(Elements.H2)(T)

    def dG9(T: Double): Double = prepareG(Elements.AlCl3)(T) + prepareG(Elements.NH3)(T) -
        prepareG(Elements.AlN)(T) - 3 * prepareG(Elements.HCl)(T)


    def dG10(T: Double): Double = prepareG(Elements.GaCl)(T) + prepareG(Elements.NH3)(T) -
        prepareG(Elements.GaN)(T) - prepareG(Elements.HCl)(T) - prepareG(Elements.H2)(T)

    def dG11(T: Double): Double = 2 * prepareG(Elements.GaCl2)(T) + 2 * prepareG(Elements.NH3)(T) -
        2 * prepareG(Elements.GaN)(T) - 4 * prepareG(Elements.HCl)(T) - prepareG(Elements.H2)(T)

    def dG12(T: Double): Double = prepareG(Elements.GaCl3)(T) + prepareG(Elements.NH3)(T) -
        prepareG(Elements.GaN)(T) - 3 * prepareG(Elements.HCl)(T)


    private def prepareK(dG: Double => Double) = (T: Double) =>
        Math.exp(-dG(T)/(Constants.R / 1e3 * T)) / Constants.Pa


    def K1(T: Double): Double = prepareK(dG1)(T)
    def K2(T: Double): Double = prepareK(dG2)(T) * Constants.Pa
    def K3(T: Double): Double = prepareK(dG3)(T) * Constants.Pa * Constants.Pa
    def K4(T: Double): Double = prepareK(dG4)(T)
    def K5(T: Double): Double = prepareK(dG5)(T) * Constants.Pa
    def K6(T: Double): Double = prepareK(dG6)(T) * Constants.Pa * Constants.Pa
    def K7(T: Double): Double = prepareK(dG7)(T)
    def K8(T: Double): Double = prepareK(dG8)(T)
    def K9(T: Double): Double = prepareK(dG9)(T)
    def K10(T: Double): Double = prepareK(dG10)(T) * Constants.Pa
    def K11(T: Double): Double = prepareK(dG11)(T)
    def K12(T: Double): Double = prepareK(dG12)(T)
}
