import java.text.DecimalFormat

import com.zieit.photometry.{CalculationResults, Calculations, OpticalDensity}
import org.specs2.matcher.Scope
import org.specs2.mutable.SpecificationWithJUnit


trait CalculationsTestContext extends Scope {
  val sequenceX: Seq[Double] = Seq(15, 20, 30, 60, 90, 120)

  val sequenceY: Seq[OpticalDensity] = Seq(
    OpticalDensity(0.0966667, 0.0966667, 0.0966667),
    OpticalDensity(0.1366667, 0.1366667, 0.1366667),
    OpticalDensity(0.1933333, 0.1933333, 0.1933333),
    OpticalDensity(0.38, 0.38, 0.38),
    OpticalDensity(0.6866667, 0.6866667, 0.6866667),
    OpticalDensity(0.8933333, 0.8933333, 0.8933333)
  )

  val calculations = new Calculations()
}

class CalculationsTest extends SpecificationWithJUnit {

  "Calculations" should {
    "calculate b " in new CalculationsTestContext {
      val calculationResult = calculations.calculateB(sequenceX, sequenceY)
      val formatter = new DecimalFormat("#0.0000000");

      formatter.format(calculationResult).toDouble must beEqualTo(0.0076818)
    }

    "multiply two sequences" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00");
      val sequenceYAvg = calculations.toOpticalDensityAverageSeq(sequenceY)
      val resultSeq = calculations.multiplySequences(sequenceX, sequenceYAvg).map(y => {
        formatter.format(y).toDouble
      })
      val refResultSequence = Seq(1.45, 2.73, 5.8, 22.8, 61.8, 107.2)

      resultSeq must beEqualTo(refResultSequence)
    }

    "calculate a" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.0000000");
      formatter.format(calculations.calculateA(sequenceX, sequenceY)).toDouble must beEqualTo(-0.0311209)
    }

    "calculate SQ" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000000");
      formatter.format(calculations.calculateSQ(sequenceX, sequenceY)).toDouble must beEqualTo(0.00357806)
    }

    "calculate SO2" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.0000000");
      formatter.format(calculations.calculateSO2(sequenceX, sequenceY)).toDouble * 10 must beEqualTo(0.008945)
    }

    "calculate b1" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.0000000");
      formatter.format(calculations.calculateB1(sequenceX, sequenceY)).toDouble must beEqualTo(0.0073044)
    }

    "calculate SQDash" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000000");
      formatter.format(calculations.calculateSQDash(sequenceX, sequenceY)).toDouble must beEqualTo(0.00545461)
    }

    "calculate SODash2" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000000");
      formatter.format(calculations.calculateSODash2(sequenceX, sequenceY)).toDouble must beEqualTo(0.00109092)
    }

    "calculate Fischer Experimental" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000");
      formatter.format(calculations.calculateFischerExp(sequenceX, sequenceY)).toDouble must beEqualTo(2.09784)
    }

    "calculate a usage based on Fischer value" in new CalculationsTestContext {
      val fischerCalculated: Double = calculations.calculateFischerExp(sequenceX, sequenceY)
      val fischerTable: Double = 7.71
      calculations.isValueAMatter(fischerCalculated, fischerTable) must beFalse
    }

    "calculate X delta" in new CalculationsTestContext {
      println(calculations.calculateXDelta(sequenceX, sequenceY))
    }

    "calculate SB" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000");
      formatter.format(calculations.calculateSB(sequenceX, sequenceY)).toDouble must beEqualTo(0.00032)
    }

    "calculate SA" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000");
      formatter.format(calculations.calculateSA(sequenceX, sequenceY)).toDouble must beEqualTo(0.04331)
    }
    "calculate SB1" in new CalculationsTestContext {
      val formatter = new DecimalFormat("#0.00000");
      formatter.format(calculations.calculateSB1(sequenceX, sequenceY)).toDouble must beEqualTo(0.00020)
    }
    "calculate XFinal" in new CalculationsTestContext {
      calculations.calculateResults(sequenceX, sequenceY) must beAnInstanceOf[CalculationResults]
    }
  }

}
