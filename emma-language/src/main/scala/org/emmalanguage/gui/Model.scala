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

import org.emmalanguage.io.csv.CSV
import org.emmalanguage.io.parquet.Parquet

object Model {
  sealed trait DataFlow
  // Sources
  case class ReadCsv(path: String, format: CSV) extends DataFlow
  case class ReadText(path: String) extends DataFlow
  case class ReadParquet(path: String, format: Parquet) extends DataFlow
  case class Ref(ref: String) extends DataFlow
  case class From(from: String) extends DataFlow

  // Transformations
  case class Map(f: String, xs: DataFlow) extends DataFlow
  case class Fold(z: String, s: String, u: String, xs: DataFlow) extends DataFlow
  case class FlatMap(f: String, xs: DataFlow) extends DataFlow
  case class Filter(p: String, xs: DataFlow) extends DataFlow
  case class GroupBy(k: String, xs: DataFlow) extends DataFlow
  case class Union(xs: DataFlow, ys: DataFlow) extends DataFlow
  case class Distinct(xs: DataFlow) extends DataFlow
  case class Fetch(xs: DataFlow) extends DataFlow
  case class Join(kx: String, ky: String, xs: DataFlow, ys: DataFlow) extends DataFlow
  case class Cross(xs: DataFlow, ys: DataFlow) extends DataFlow

  // Sinks
  case class Bind(bind: String, xs: DataFlow) extends DataFlow
  case class WriteCsv(path: String, format: CSV, xs: DataFlow) extends DataFlow
  case class WriteText(path: String, xs: DataFlow) extends DataFlow
  case class WriteParquet(path: String, format: Parquet, xs: DataFlow) extends DataFlow
}
