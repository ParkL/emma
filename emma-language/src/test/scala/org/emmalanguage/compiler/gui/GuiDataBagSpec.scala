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
package compiler.gui

import org.emmalanguage.api.DataBag
import org.emmalanguage.compiler.BaseCompilerSpec
import org.emmalanguage.gui.GuiDataBag
import org.emmalanguage.gui.Model
import org.emmalanguage.io

class GuiDataBagSpec extends BaseCompilerSpec {

  import compiler._

  lazy val liftCombine: u.Expr[Any] => u.Tree =
    pipeline(typeCheck = true)(
      Core.lift,
      Comprehension.combine,
      Core.flatten
    ).compose(_.tree)

  lazy val liftCombineInsert: u.Expr[Any] => u.Tree =
    pipeline(typeCheck = true)(
      Core.lnf,
      Comprehension.combine,
      Core.flatten,
      tree => time(Backend.addGuiDatabagCalls(tree), "GuiDataBag Calls")
    ).compose(_.tree)

  "GuiDataBagSpec" - {
    import Model._
    import JsonProtocol._
    import io.csv._
    import io.parquet._

    import spray.json._
    """GuiDataBag-Object splice""" in {
      val act = liftCombineInsert(u.reify {
        val db1 = DataBag(1 to 10)
        val db2 = DataBag(11 to 20)

        val rs1 = db1.map(_ * 2)
        val rs2 = db2.map(_ * 3)
        val rs3 = rs1 union rs2

        rs1.writeText("rs1path")
        rs2.writeText("rs2path")
        rs3.writeText("rs3path")
      })

      val exp = liftCombine(u.reify {
        val db1$2 = GuiDataBag(1 to 10)
        val db2$1 = GuiDataBag(11 to 20) // 1 use

        val rs1$2 = db1$2.map(_ * 2)
        val rs1$1 = GuiDataBag.ref(rs1$2, "rs1")
        val rs2$2 = db2$1.map(_ * 3)
        val rs2$1 = GuiDataBag.ref(rs2$2, "rs2")
        val rs3 = rs1$1 union rs2$1

        rs1$1.writeText("rs1path")
        rs2$1.writeText("rs2path")
        rs3.writeText("rs3path")
      })

      act shouldBe alphaEqTo(exp)
    }
    """Marshaling / unmarshaling""" in {
      val map: DataFlow = Map("map", ReadText("textPath"))
      val json = map.toJson
      json.convertTo[DataFlow] shouldBe map
    }
    """Complex marshaling example""" in {
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

      val flows: List[DataFlow] = List(
        Bind("bind", fetch),
        WriteCsv("csvPath", format = CSV(), fetch),
        WriteText("textPath", fetch),
        WriteParquet("parquetPath", format = Parquet(), fetch)
      )

      for {
        f <- flows
        json = f.toJson
      } {
        println(s"Json for: $f")
        println(json.prettyPrint)
        json.convertTo[DataFlow] shouldBe f
      }
    }
  }
}
