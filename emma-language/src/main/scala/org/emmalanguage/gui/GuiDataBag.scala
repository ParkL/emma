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

import scala.language.higherKinds
import api._
import org.emmalanguage.io.csv.CSV
import org.emmalanguage.io.csv.CSVConverter
import org.emmalanguage.io.parquet.Parquet
import org.emmalanguage.io.parquet.ParquetConverter
import Model._
import org.emmalanguage.io.csv.CSVScalaSupport
import org.emmalanguage.io.parquet.ParquetScalaSupport
import org.emmalanguage.io.text.TextSupport

// TODO typ rep : case class Rep(DataBag, Node)
// TODO refactor: -> 1 arg `rep`
class GuiDataBag[A](val dataBag: DataBag[A], val node: DataFlow)(implicit gui: GuiEnv) extends DataBag[A] {

  @transient override implicit def m: Meta[A] = dataBag.m

  override def fold[B: Meta](z: B)(s: (A) => B, u: (B, B) => B): B = {
    gui.renderAndHalt(Fold("z", "s", "u", node))
    dataBag.fold(z)(s, u)
  }

  override def map[B: Meta](f: (A) => B): DataBag[B] =
    new GuiDataBag(dataBag.map(f), Map("f", node))

  override def flatMap[B: Meta](f: (A) => DataBag[B]): DataBag[B] =
    new GuiDataBag(dataBag.flatMap(f), FlatMap("f", node))

  override def withFilter(p: (A) => Boolean): DataBag[A] =
    new GuiDataBag(dataBag.withFilter(p), Filter("f", node))

  override def groupBy[K: Meta](k: (A) => K): DataBag[Group[K, DataBag[A]]] =
    new GuiDataBag(dataBag.groupBy(k), GroupBy("f", node))

  override def union(that: DataBag[A]): DataBag[A] = that match {
    case that: GuiDataBag[A] => new GuiDataBag(this.dataBag union that.dataBag, Union(node, that.node))
  }

  override def distinct: DataBag[A] =
    new GuiDataBag(dataBag.distinct, Distinct(node))

  override def writeCSV(path: String, format: CSV)(implicit converter: CSVConverter[A]): Unit = {
    // effect -> render -> send to gui -> wait for gui -> gui remote control -> Terminate
    gui.renderAndHalt(WriteCsv(path, format, node))
    dataBag.writeCSV(path, format)
  }

  override def writeText(path: String): Unit = {
    // effect -> render -> send to gui -> wait for gui -> gui remote control -> terminate
    gui.renderAndHalt(WriteText(path, node))
    dataBag.writeText(path)
  }

  override def writeParquet(path: String, format: Parquet)(implicit converter: ParquetConverter[A]): Unit = {
    // effect -> render -> send to gui -> wait for gui -> gui remote control -> terminate
    gui.renderAndHalt(WriteText(path, node))
    dataBag.writeParquet(path, format)
  }

  override def fetch(): Seq[A] = {
    // effect -> render -> send to gui -> wait for gui -> gui remote control
    gui.renderAndHalt(Fetch(node))
    dataBag.fetch()
  }
}

// TODO all static methods to environemtn, then delegate to proper backend
object GuiDataBag extends DataBagCompanion[GuiEnv] {

  implicit object GeTemp extends GuiEnv {
    type E = Nothing
    implicit val rt = ???
    val ops = ???
    val companion = ???
  }

  def empty[A: Meta](
    implicit env: GuiEnv
  ): DataBag[A] = new GuiDataBag[A](env.empty[A], Ref("empty"))

  def apply[A: Meta](values: Seq[A])(
    implicit env: GuiEnv
  ): DataBag[A] = new GuiDataBag[A](env.apply(values), Ref("Seq"))

  def readText(path: String)(
    implicit env: GuiEnv
  ): DataBag[String] = new GuiDataBag[String](env.readText(path), ReadText(path))

  def readCSV[A: Meta : CSVConverter](path: String, format: CSV)(
    implicit env: GuiEnv
  ): DataBag[A] = new GuiDataBag[A](env.readCSV(path, format), ReadCsv(path, format))

  def readParquet[A: Meta : ParquetConverter](path: String, format: Parquet)(
    implicit env: GuiEnv
  ): DataBag[A] = new GuiDataBag[A](env.readParquet(path, format), ReadParquet(path, format))

//  override def from[DColl[_], A](coll: DColl[A])(implicit conv: DColl[A] => DataBag[A]): DataBag[A] =
//    new GuiDataBag[A](DataBag.from(coll), Ref("external")) // TODO

  def ref[A: Meta](dataBag: DataBag[A], ref: String)(implicit env: GuiEnv): DataBag[A] =
    new GuiDataBag(dataBag, Ref(ref))

  def cross[A: Meta, B: Meta](xs: DataBag[A], ys: DataBag[B])
                             (c: (DataBag[A], DataBag[B]) => DataBag[(A, B)])
                             (implicit env: GuiEnv): DataBag[(A, B)] = {
    env.cross(xs, ys)
  }

  def equiJoin[A: Meta, B: Meta, K: Meta](keyx: A => K, keyy: B => K)(xs: DataBag[A], ys: DataBag[B])
                                         (j: (A => K, B => K) => (DataBag[A], DataBag[B]) => DataBag[(A, B)])
                                         (implicit guiClient: GuiEnv): DataBag[(A, B)] = (xs, ys) match {
    case (xsGdb: GuiDataBag[A], ysGdb: GuiDataBag[B]) =>
      new GuiDataBag(
        dataBag = j(keyx, keyy)(xs, ys),
        node = Join("kx", "ky", xsGdb.node, ysGdb.node)
      )
  }
}
