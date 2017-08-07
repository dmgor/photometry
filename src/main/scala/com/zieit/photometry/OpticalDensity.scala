package com.zieit.photometry

/**
  * Created by dmitriyg on 3/18/17.
  */
case class OpticalDensity(firstReadout: Double, secondReadout: Double, thirdReadout: Double) {

  def avg: Double = {
    (firstReadout + secondReadout + thirdReadout) / 3
  }

}

case class OpticalDensities(measurements: Seq[OpticalDensity])

case class XValues(xvalues: Seq[String]) {
  def toDoubleSeq = xvalues.map(x => x.toDouble)
}

case class FullDataObject(measurements: Seq[OpticalDensity], xvalues: Seq[String]) {
  def toDoubleSeq = xvalues.map(x => x.toDouble)
}
