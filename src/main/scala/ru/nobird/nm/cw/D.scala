package ru.nobird.nm.cw

/**
  * Created by ruslandavletshin on 30/05/2017.
  */
object D {

    private def sigma(e: Element) = (e.sigma + Elements.N2.sigma) / 2

    private def eps(e: Element) = Math.sqrt(e.eps * Elements.N2.eps)

    private def mu(e: Element) = 2 * e.mu * Elements.N2.mu / (e.mu + Elements.N2.mu)

    private def omega(x: Double) = 1.074 * Math.pow(x, -0.1604)

    private def prepareD(e: Element) = (T: Double) =>
        2.628 * 0.01 * Math.pow(T, 3 / 2) / (Constants.Pa * sigma(e) * omega(T / eps(e)) * Math.sqrt(mu(e)))


    def D_AlCl(T: Double) : Double = prepareD(Elements.AlCl)(T)
    def D_AlCl2(T: Double) : Double = prepareD(Elements.AlCl2)(T)
    def D_AlCl3(T: Double) : Double = prepareD(Elements.AlCl3)(T)

    def D_GaCl(T: Double) : Double = prepareD(Elements.GaCl)(T)
    def D_GaCl2(T: Double) : Double = prepareD(Elements.GaCl2)(T)
    def D_GaCl3(T: Double) : Double = prepareD(Elements.GaCl3)(T)

    def D_NH3(T: Double) : Double = prepareD(Elements.NH3)(T)
    def D_H2(T: Double) : Double = prepareD(Elements.H2)(T)
    def D_HCl(T: Double) : Double = prepareD(Elements.HCl)(T)

    def D_N2(T: Double) : Double = prepareD(Elements.N2)(T)
}