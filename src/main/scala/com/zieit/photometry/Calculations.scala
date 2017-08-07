package com.zieit.photometry

import java.text.DecimalFormat


class Calculations {

  def calculateB(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {

    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)

    val experimentNumber = sequenceX.size

    val seqXonSeqYAvg = multiplySequences(sequenceX, sequenceYAvg)

    val calcBUpper = (experimentNumber * multiplySequences(sequenceX, sequenceYAvg).sum) - (sequenceX.sum * sequenceYAvg.sum)

    val calcBLower = (experimentNumber * sequenceX.map(x => x * x).sum) - (sequenceX.sum * sequenceX.sum)

    calcBUpper / calcBLower
  }

  def calculateA(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    val calcAUpper = sequenceYAvg.sum - (calculateB(sequenceX, sequenceY) * sequenceX.sum)
    calcAUpper / experimentNumber
  }

  def calculateSQ(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    val sequenceYAvgSumSquare = sequenceYAvg.map(y => y * y).sum
    sequenceYAvgSumSquare - (calculateA(sequenceX, sequenceY) * sequenceYAvg.sum) - ((calculateB(sequenceX, sequenceY)) * (multiplySequences(sequenceX, sequenceYAvg)).sum)
  }

  def calculateSO2(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    calculateSQ(sequenceX, sequenceY) / (experimentNumber - 2)
  }

  def calculateSODash2(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    calculateSQDash(sequenceX, sequenceY) / (experimentNumber - 1)
  }

  def calculateSQDash(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    val sequenceYAvgSumSquare = sequenceYAvg.map(y => y * y).sum
    sequenceYAvgSumSquare - calculateB1(sequenceX, sequenceY) * (multiplySequences(sequenceX, sequenceYAvg)).sum
  }

  def calculateB1(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    (multiplySequences(sequenceX, sequenceYAvg).sum) / multiplySequences(sequenceX, sequenceX).sum
  }

  def multiplySequences(sequenceX: Seq[Double], sequenceY: Seq[Double]): Seq[Double] = {

    sequenceX.indices.map(i => sequenceX(i) * sequenceY(i))
  }

  def toOpticalDensityAverageSeq(sequenceY: Seq[OpticalDensity]): Seq[Double] = {
    sequenceY.map(_.avg)
  }

  def calculateFischerExp(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    (calculateSQDash(sequenceX, sequenceY) - calculateSQ(sequenceX, sequenceY)) / calculateSO2(sequenceX, sequenceY)
  }

  def isValueAMatter(fischerCalculated: Double, fischerTable: Double): Boolean = {
    fischerTable < fischerCalculated
  }

  def calculateXDelta(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]) = {
    val fischerTable = 7.71
    val fischerExp = calculateFischerExp(sequenceX, sequenceY)

    def calXmiddle(x: Double) =
      isValueAMatter(fischerExp, fischerTable) match {
        case false => {
          (1 / calculateB(sequenceX, sequenceY)) * x
        }
      }

    sequenceX.map(x => calXmiddle(x))
  }

  def toCalculationFunction(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): CalculationFunction = {
    val fischerTable = 7.71
    val fischerExp = calculateFischerExp(sequenceX, sequenceY)

    new CalculationFunction(isValueAMatter(fischerExp, fischerTable))
  }

  def calculateSBSquare(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    val sbSquare = (experimentNumber * calculateSO2(sequenceX, sequenceY)) / ((experimentNumber * multiplySequences(sequenceX, sequenceX).sum) - Math.pow(sequenceX.sum, 2))
    sbSquare
  }

  def calculateSB(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    math.sqrt(calculateSBSquare(sequenceX, sequenceY))
  }

  def calculateSA(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    val sa = math.sqrt(calculateSBSquare(sequenceX, sequenceY) * Math.pow(sequenceX.sum, 2) / experimentNumber)
    println(sa)
    sa
  }

  def calculateDeltaA(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    2.6 * calculateSA(sequenceX, sequenceY)
  }

  def calculateDeltaB(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    2.6 * calculateSB(sequenceX, sequenceY)
  }

  def calculateSB1(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val sb1 = Math.sqrt(calculateSODash2(sequenceX, sequenceY) / multiplySequences(sequenceX, sequenceX).sum)
    sb1
  }

  def calculateDeltaB1(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    2.6 * calculateSB1(sequenceX, sequenceY)
  }

  def calculateCorrelationCoef(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): Double = {
    val experimentNumber = sequenceX.size
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    val sequenceXSumSquare = sequenceX.map(x => x * x).sum
    val sequenceYAvgSumSquare = sequenceYAvg.map(y => y * y).sum
    val r = (experimentNumber * multiplySequences(sequenceX,sequenceYAvg).sum - (sequenceX.sum * sequenceYAvg.sum))/ math.sqrt((experimentNumber*sequenceXSumSquare-(sequenceX.sum*sequenceX.sum))*(experimentNumber*sequenceYAvgSumSquare-(sequenceYAvg.sum*sequenceYAvg.sum)))
    r
  }

  def calculateResults(sequenceX: Seq[Double], sequenceY: Seq[OpticalDensity]): CalculationResults = {
    val sequenceYAvg = toOpticalDensityAverageSeq(sequenceY)
    val b1 = formatDouble(calculateB1(sequenceX, sequenceY))
    val b = formatDouble(calculateB(sequenceX, sequenceY))
    val a = formatDouble(calculateA(sequenceX, sequenceY))
    val calculationFunction = toCalculationFunction(sequenceX, sequenceY)

    def xCalculated = calculationFunction.isAMatter match {
      case false => sequenceYAvg.map(yAvg => yAvg / b1)
      case true => sequenceYAvg.map(yAvg => yAvg / b - a / b)
    }

    CalculationResults(
      xCalculated = xCalculated,
      yAverage = sequenceYAvg,
      xvalues = sequenceX,
      calculationFunctionDef = calculationFunction.toDescription,
      calculationFunctionParsed = calculationFunction.toParsedFormula(a, b, b1),
      a = a,
      b = b,
      b1 = b1,
      SOSquare = formatDouble(calculateSO2(sequenceX, sequenceY)),
      SQDash = formatDouble(calculateSQDash(sequenceX, sequenceY)),
      SB1 = formatDouble(calculateSB1(sequenceX, sequenceY)),
      DeltaB1 = formatDouble(calculateDeltaB1(sequenceX, sequenceY)),
      DeltaA = formatDouble(calculateDeltaA(sequenceX, sequenceY)),
      SQ = formatDouble(calculateSQ(sequenceX, sequenceY)),
      SODash2 = formatDouble(calculateSODash2(sequenceX, sequenceY)),
      CorrelationCoef=formatDouble(calculateCorrelationCoef(sequenceX,sequenceY)))
  }

  def calculateDeltaErrorPercent(sequenceX: Seq[Double], sequenceXFinal: Seq[Double]): Seq[Double] = {
    sequenceX.indices.map(i => formatDouble((math.abs(sequenceX(i) - sequenceXFinal(i))) / sequenceX(i) / 100))
  }

  def formatDouble(value: Double): Double = {
    val formatter = new DecimalFormat("#0.000000")
    formatter.format(value).toDouble
  }

}
