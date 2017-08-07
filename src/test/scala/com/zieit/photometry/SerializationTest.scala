import com.zieit.photometry._
import org.specs2.matcher.Scope
import org.specs2.mutable.SpecificationWithJUnit

/**
  * Created by dmitriyg on 4/30/17.
  */
class SerializationTest extends SpecificationWithJUnit {

  val jsonDensitiesString: String =
    """{
      |"measurements":
      |[
      |{
      |"firstReadout":"1.1",
      |"secondReadout": "68",
      |"thirdReadout": "77.987"},
      |{"firstReadout":"1.12",
      |"secondReadout": "61.8",
      |"thirdReadout": "77.987"}
      |]
      |}""".stripMargin

  val jsonXValuesString: String =
    """{
      |"xvalues":
      |["12.1","20.0","30.0","60.1","90.1","120.2"]
      |}""".stripMargin

  val fullJsonString: String =
    """{
      |"measurements":
      |[
      |{
      |"firstReadout":"1.1",
      |"secondReadout": "68",
      |"thirdReadout": "77.987"},
      |{"firstReadout":"1.12",
      |"secondReadout": "61.8",
      |"thirdReadout": "77.987"}
      |],
      |"xvalues":
      |["12.1","20.0"]
      |}""".stripMargin

  val opticalDensities: OpticalDensities = OpticalDensities(Seq(OpticalDensity(1.1, 68, 77.987), OpticalDensity(1.12, 61.8, 77.987)));
  val xValues = Seq(12.1, 20.0, 30.0, 60.1, 90.1, 120.2)

  "jsonDeSeRialization" should {
    "deserialize measurements" in new Scope {
      JsonUtil.fromJson[OpticalDensities](jsonDensitiesString) mustEqual opticalDensities
    }

    "deserialize xValues" in new Scope {
      val desValues = JsonUtil.fromJson[XValues](jsonXValuesString)
      desValues.xvalues.head must beEqualTo(xValues.head.toString)

    }

    "deserrializeFullJson" in new Scope {
      val fullData = JsonUtil.fromJson[FullDataObject](fullJsonString)
      fullData.measurements mustEqual (opticalDensities.measurements)
    }

  }
}
