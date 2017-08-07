package com.zieit.photometry

/**
  * Created by dmitriyg on 5/5/17.
  */
case class CalculationResults(xCalculated: Seq[Double],
                              yAverage: Seq[Double],
                              xvalues: Seq[Double],
                              calculationFunctionDef: String,
                              calculationFunctionParsed: String,
                              a:Double,
                              b:Double,
                              b1:Double,
                              SOSquare:Double,
                              SQDash:Double,
                              SB1:Double,
                              DeltaB1:Double,
                              DeltaA:Double,
                              SQ:Double,
                              SODash2:Double,
                              CorrelationCoef:Double
                             ) {

}
