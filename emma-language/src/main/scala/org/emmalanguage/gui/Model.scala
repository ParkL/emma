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
  type IdType = java.util.UUID
  type IdAndRep = (IdType, String)

  sealed trait DataFlow {
    // TODO make map type -> int for ids
    //lazy val id: IdType = java.util.UUID.randomUUID()
    def flatString(): String = s"$toString"
    def idAndString(): (IdType, String) = java.util.UUID.randomUUID() -> flatString()
    def children(): List[DataFlow] = List.empty

    def mkGraph(flow: DataFlow): Set[(IdAndRep, IdAndRep)] = {
      val kids = flow.children()
      val lx = for { k <- kids } yield { (flow.idAndString(), k.idAndString())  }
      lx.toSet ++ kids.flatMap(mkGraph)
    }

    def mkGraph(): Set[(IdAndRep, IdAndRep)] = mkGraph(this)
  }
  // Sources
  case class ReadCsv(path: String, format: CSV) extends DataFlow
  case class ReadText(path: String) extends DataFlow
  case class ReadParquet(path: String, format: Parquet) extends DataFlow
  case class Ref(ref: String) extends DataFlow
  case class From(from: String) extends DataFlow

  // Transformations
  case class Map(f: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Map($f)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class Fold(z: String, s: String, u: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Fold($z, $s, $u)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class FlatMap(f: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"FlatMap($f)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class Filter(p: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Filter($p)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class GroupBy(k: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"GroupBy($k)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class Union(xs: DataFlow, ys: DataFlow) extends DataFlow {
    override def flatString(): String = s"Union"

    override def children(): List[DataFlow] = List(xs, ys)
  }
  case class Distinct(xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Distinct"

    override def children(): List[DataFlow] = List(xs)
  }
  case class Fetch(xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Fetch"

    override def children(): List[DataFlow] = List(xs)
  }
  case class Join(kx: String, ky: String, xs: DataFlow, ys: DataFlow) extends DataFlow {
    override def flatString(): String = s"Join($kx, $ky)"

    override def children(): List[DataFlow] = List(xs, ys)
  }
  case class Cross(xs: DataFlow, ys: DataFlow) extends DataFlow {
    override def flatString(): String = s"Cross"

    override def children(): List[DataFlow] = List(xs, ys)
  }

  // Sinks
  case class Bind(bind: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"Bind($bind)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class WriteCsv(path: String, format: CSV, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"WriteCsv($path, format)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class WriteText(path: String, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"WriteText($path)"

    override def children(): List[DataFlow] = List(xs)
  }
  case class WriteParquet(path: String, format: Parquet, xs: DataFlow) extends DataFlow {
    override def flatString(): String = s"WriteParquet($path)"

    override def children(): List[DataFlow] = List(xs)
  }
}
