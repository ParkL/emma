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
package compiler.lang.backend

import compiler.Common
import compiler.lang.core.Core

import shapeless._

private[backend] trait Debug extends Common { self: Backend with Core =>

  private[backend] object Debug {
    import Core.{Lang => core}
    import UniverseImplicits._

    // TODO cross, equiJoin
    lazy val addGuiDatabagCalls: u.Tree => u.Tree =
      api.BottomUp.withUses.withRoot.transformSyn {
        // val rs1 = db1.map(_ * 2) if uses of the symbol > 1
        //   =>
        // val rs1$2 = db1$2.map(_ * 2)
        // val rs1$1 = GuiDataBag.ref(rs1$2, "rs1")
        case Attr(core.ValDef(lhs, rhs), _, Some(root) :: _, syn)
          if syn(root).head(lhs) > 1 =>
          val lhsOp = api.TermSym(
            lhs,
            api.TermName.fresh(lhs),
            lhs.info,
            pos = lhs.pos
          )
          val refCall = core.DefCall(
            Some(core.Ref(API.GuiDataBag$.sym)),
            API.GuiDataBag$.ref,
            Seq(rhs.tpe.typeArgs.head),
            Seq(Seq(core.Ref(lhsOp), core.Lit(lhs.name.toString)))
          )
          core.ValDef(lhs, core.Let(
            Seq(
              core.ValDef(lhsOp, rhs),
              core.ValDef(lhs, refCall)
            ),
            Seq.empty,
            core.Ref(lhs)))

        // Redirect calls to the Bag Module
        // DatBag.apply(1 to 10) => GuiDataBag(1 to 10)
        case Attr.none(core.DefCall(Some(core.Ref(sym)), member, tpes, argss))
          if sym == API.DataBag$.sym && (API.DataBag$.ops contains member) =>
          core.DefCall(
            Some(core.Ref(API.GuiDataBag$.sym)),
            API.GuiDataBag$.opsByName(member.name),
            tpes,
            argss
          )
      }._tree andThen Core.flatten
    }
}
