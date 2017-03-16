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
    """Debug Wordcount""" in {
      val input: String = null
      val output: String = null

      val act = liftCombineInsert(u.reify{
        val docs = DataBag.readText(input)
        val words = docs.map(_.toLowerCase().split("\\W+"))
        val counts = words.groupBy(Predef.identity).map(g => (g.key, g.values.size))
        counts.writeText(output)
      })

      val act2 = liftCombineInsert(u.reify{
        val words = for {
          line <- DataBag.readText(input)
          word <- DataBag(line.toLowerCase.split("\\W+"))
        } yield word

        val counts = for {
          group <- words.groupBy(Predef.identity)
        } yield (group.key, group.values.size)
        counts.writeText(output)
      })

      val exp = liftCombine(u.reify{
        val docs = GuiDataBag.readText(input)
        val words = docs.map(_.toLowerCase().split("\\W+"))
        val counts = words.groupBy(Predef.identity).map(g => (g.key, g.values.size))
        counts.writeText(output)
      })

      act shouldBe alphaEqTo(exp)
      act2 shouldBe a [u.Tree]
    }
  }
}
