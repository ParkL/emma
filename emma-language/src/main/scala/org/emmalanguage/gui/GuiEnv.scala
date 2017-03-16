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
package gui

import org.emmalanguage.api.DataBag
import org.emmalanguage.api.DataBagCompanion
import org.emmalanguage.api.Meta
import org.emmalanguage.api.backend.ComprehensionCombinators
import org.emmalanguage.api.backend.Runtime
import org.emmalanguage.gui.Model.Cross
import org.emmalanguage.gui.Model.DataFlow

import io.csv._
import io.parquet._
import io.text._

trait GuiEnv {
  type E
  implicit val rt: E

  def empty[A: Meta](implicit env: E): DataBag[A] =
    companion.empty[A]

  def apply[A: Meta](values: Seq[A])(implicit env: E): DataBag[A] =
    companion.apply(values)

  def readText(path: String)(implicit env: E): DataBag[String] =
    companion.readText(path)

  def readCSV[A: Meta : CSVConverter](path: String, format: CSV)(implicit env: E): DataBag[A] =
    companion.readCSV[A](path, format)

  def readParquet[A: Meta : ParquetConverter](path: String, format: Parquet)(implicit env: E): DataBag[A] =
    companion.readParquet(path, format)

  def cross[A, B](
    xs: DataBag[A], ys: DataBag[B]
  )(implicit A: Meta[A], B: Meta[B]): DataBag[(A, B)] =
    (xs, ys) match {
      case (xs: GuiDataBag[A], ys: GuiDataBag[B]) => new GuiDataBag(
        dataBag = ops.cross(xs, ys),
        node = Cross(xs.node, ys.node)
      )
    }

  val ops: ComprehensionCombinators[E] with Runtime[E]
  val companion: DataBagCompanion[E]
  // effect -> render -> send to gui -> wait for gui (block) -> gui "remote control" -> next transformation
  def renderAndHalt[A](dataBagNode: DataFlow): Unit = ???
}
