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
  sealed trait DataFlow {
    type Edge = (DataFlow, DataFlow)
//    def nodes(df: DataFlow): List[DataFlow] = {
//      df match {
//        case r: ReadCsv => List(r)
//        case r: ReadText => List(r)
//        case r: ReadParquet => List(r)
//        case r: Ref => List(r)
//        case f: From => List(f)
//
//        case m @ Map(f, xs) => List(m) ::: nodes(xs).flatten
//        case f @ Fold(z, s, u, xs) => List(f) ::: nodes(xs).flatten
//        case fm @ FlatMap(f, xs) => List(fm) ::: nodes(xs).flatten
//        case f @ Filter(p, xs) => List(f) ::: nodes(xs).flatten
//        case gb @ GroupBy(k, xs) => List(gb) ::: nodes(xs).flatten
//        case u @ Union(xs, ys) => List(u) ::: nodes(xs).flatten ::: nodes(ys).flatten
//        case d @ Distinct(xs) => List(d) ::: nodes(xs).flatten
//        case f @ Fetch(xs) => List(f) ::: nodes(xs).flatten
//        case j @ Join(kx, ky, xs, ys) => List(j) ::: nodes(xs).flatten ::: nodes(ys).flatten
//        case c @ Cross(xs, ys) => List(c) ::: nodes(xs).flatten ::: nodes(ys).flatten
//        case b @ Bind(bind, xs) => List(b) ::: nodes(xs).flatten
//        case w @ WriteCsv(path, format, xs) => List(w) ::: nodes(xs).flatten
//        case w @ WriteText(path, xs) => List(w) ::: nodes(xs).flatten
//        case w @ WriteParquet(path, format, xs) => List(w) ::: nodes(xs).flatten
//      }
//    }

    def mkGraph(df: DataFlow): Set[Edge] = {
      df match {
        case ReadCsv(path, format) => Set.empty[Edge]
        case ReadText(path) => Set.empty[Edge]
        case ReadParquet(path, format) => Set.empty[Edge]
        case Ref(ref) => Set.empty[Edge]
        case From(from) => Set.empty[Edge]

        case t @ Map(f, xs) => Set((t, xs)) ++ mkGraph(xs)
        case f @ Fold(z, s, u, xs) => Set((f, xs)) ++ mkGraph(xs)
        case fm @ FlatMap(f, xs) => Set((fm, xs)) ++ mkGraph(xs)
        case f @ Filter(p, xs) => Set((f, xs)) ++ mkGraph(xs)
        case gb @ GroupBy(k, xs) => Set((gb, xs)) ++ mkGraph(xs)
        case u @ Union(xs, ys) => Set((u, xs), (u, ys)) ++ mkGraph(xs) ++ mkGraph(ys)
        case d @ Distinct(xs) => Set((d, xs)) ++ mkGraph(xs)
        case f @ Fetch(xs) => Set((f, xs)) ++ mkGraph(xs)
        case j @ Join(kx, ky, xs, ys) => Set((j, xs), (j, ys)) ++ mkGraph(xs) ++ mkGraph(ys)
        case c @ Cross(xs, ys) => Set((c, xs), (c, ys)) ++ mkGraph(xs) ++ mkGraph(ys)

        case b @ Bind(bind, xs) => Set((b, xs)) ++ mkGraph(xs)
        case w @ WriteCsv(path, format, xs) => Set((w, xs)) ++ mkGraph(xs)
        case w @ WriteText(path, xs) => Set((w, xs)) ++ mkGraph(xs)
        case w @ WriteParquet(path, format, xs) => Set((w, xs)) ++ mkGraph(xs)
      }
    }
    def mkGraph(): Set[Edge] = mkGraph(this)
  }
  // Sources
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
