package com.zieit.photometry

/**
  * Created by dmitriyg on 6/4/17.
  */
case class CalculationFunction(isAMatter:Boolean) {


  def toDescription : String = {
    isAMatter match {
      case true => "y = a + bx"
      case false => "y = b1x "
    }
  }

  def toParsedFormula(a:Double,b:Double,b1:Double):String = {
    isAMatter match {
      case true => "y ="+ a.toString + " + "+ b.toString+"x"
      case false => "y = "+b1+"x"
    }
  }
}
