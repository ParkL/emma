/*
 * Copyright © 2014 TU Berlin (emma@dima.tu-berlin.de)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.emmalanguage
package shell

import org.emmalanguage.shell.util.GraphEasySupport

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import spray.json._

class MarshalingSpec
  extends FreeSpec
    with Matchers
    with ScalatestRouteTest
    with LineModel {

  import gui.Model._
  import io.parquet._
  import io.csv._

  lazy val testFlows: List[DataFlow] = {
    def mkCrazyFlow(source: DataFlow): DataFlow = {
      val map = Map(f = "mapFun", xs = source)
      val fold = Fold(z = "z", s = "s", u = "u", xs = map)
      val flatMap = FlatMap(f = "flatMapFun", xs = fold)
      val filter = Filter(p = "pred", xs = flatMap)
      val groupBy = GroupBy(k = "keyFun", xs = filter)
      Distinct(groupBy)
    }

    val fromText = mkCrazyFlow(ReadText(path = "readTextPath"))
    val fromCsv = mkCrazyFlow(ReadCsv(path = "readCsvPath", format = CSV()))
    val fromRef = mkCrazyFlow(Ref(ref = "ref"))
    val fromParquet = mkCrazyFlow(ReadParquet(path = "readParquetPath", format = Parquet()))

    val unionTextCsv = Union(xs = fromText, ys = fromCsv)
    val joinWithRef = Join(kx = "kx", ky = "ky", xs = unionTextCsv, ys = fromRef)
    val crossWithParquet = Cross(joinWithRef, fromParquet)
    val fetch = Fetch(crossWithParquet)

    List(
      Bind("bind", fetch),
      WriteCsv("csvPath", format = CSV(), fetch),
      WriteText("textPath", fetch),
      WriteParquet("parquetPath", format = Parquet(), fetch)
    )
  }

  """Marshaling / unmarshaling""" in {
    val map: DataFlow = Map("map", ReadText("textPath"))
    val json = map.toJson
    json.convertTo[DataFlow] shouldBe map
  }
  """Complex marshaling example""" in {
    for {
      f <- testFlows
      json = f.toJson
    } {
      println(s"Json for: $f")
      println(json.prettyPrint)
      json.convertTo[DataFlow] shouldBe f
    }
  }
  """mkGraph and GraphEasySupport""" in {
    val graph = testFlows.head.mkGraph()
    for (print <- GraphEasySupport.printer() ) print(graph)
  }

  """Simple route test""" in {
    Get("/test") ~> Shell.route ~> check {
      status shouldBe StatusCodes.OK
      println(responseAs[String])
      responseAs[DataFlow] shouldBe Shell.flow
    }
  }
}
