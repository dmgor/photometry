import com.zieit.photometry.OpticalDensity
import org.specs2.matcher.Scope
import org.specs2.mutable.SpecificationWithJUnit

/**
  * Created by dmitriyg on 3/18/17.
  */
class OpticalDensityTest extends SpecificationWithJUnit {

  "OpticalDensity" should {
    "return average " in new Scope {

      val od1 = new OpticalDensity(0.0966667,0.0966667,0.0966667)
      val od2 = new OpticalDensity(0.1366667,0.1366667,0.1366667)

      od1.avg must beEqualTo(0.0966667)
      od2.avg must beEqualTo(0.1366667)

    }
  }

}
