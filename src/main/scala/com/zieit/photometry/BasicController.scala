package com.zieit.photometry

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller


class BasicController extends Controller {

  get("/:*") { request: Request =>
    response.ok.fileOrIndex(
      filePath = request.params("*"),
      indexPath = "index.html")
  }

  post("/dencitypost") {
    request: Request =>
      val fullJsonString = request.contentString
      val fullData = JsonUtil.fromJson[FullDataObject](fullJsonString)
      //val seqX = fullData.toDoubleSeq
      //val seqY = fullData.measurements

      val seqX : Seq[Double] = Seq(15,20,30,60,90,120)

      val seqY : Seq[OpticalDensity] = Seq(
        OpticalDensity(0.0966667,0.0966667,0.0966667),
        OpticalDensity(0.1366667,0.1366667,0.1366667),
        OpticalDensity(0.1933333,0.1933333,0.1933333),
        OpticalDensity(0.38,0.38,0.38),
        OpticalDensity(0.6866667,0.6866667,0.6866667),
        OpticalDensity(0.8933333,0.8933333,0.8933333)
      )
      val calculations = new Calculations()
      val seqYAverage = calculations.toOpticalDensityAverageSeq(seqY)
      val results =  calculations.calculateResults(seqX,seqY)
      JsonUtil.toJson(results)
  }

}
