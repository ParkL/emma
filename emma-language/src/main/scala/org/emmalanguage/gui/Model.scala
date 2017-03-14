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

import org.emmalanguage.api.Meta
import org.emmalanguage.io.csv.CSV
import org.emmalanguage.io.parquet.Parquet

import spray.json.DefaultJsonProtocol

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


  object JsonProtocol extends DefaultJsonProtocol {
    import spray.json._

    // TODO make proper format
    implicit object ParquetFormat extends JsonFormat[Parquet] {
      override def read(json: JsValue): Parquet = json match {
        case JsString("parquet") => Parquet() // FIXME to default atm only
        case x => deserializationError(s"""Expected "parquet" got: $x""")
      }
      override def write(obj: Parquet): JsValue = JsString("parquet")
    }

    implicit object CSVFormat extends JsonFormat[CSV] {
      override def read(json: JsValue): CSV = json match {
        case JsString("csv") => CSV() // FIXME to default atm only
        case x => deserializationError(s"""Expected "csv" got: $x""")
      }
      override def write(obj: CSV): JsValue = JsString("csv")
    }

    implicit object DataBagNodeFormat extends RootJsonFormat[DataFlow] {
      implicit val readCsvFormat: RootJsonFormat[ReadCsv] = jsonFormat2(ReadCsv)
      implicit val readTextFormat: RootJsonFormat[ReadText] = jsonFormat1(ReadText)
      implicit val readParquetFormat: RootJsonFormat[ReadParquet] = jsonFormat2(ReadParquet)
      implicit val refFormat: RootJsonFormat[Ref] = jsonFormat1(Ref)
      implicit val fromFormat: RootJsonFormat[From] = jsonFormat1(From)

      implicit val mapFormat: RootJsonFormat[Map] = jsonFormat2(Map)
      implicit val foldFormat: RootJsonFormat[Fold] = jsonFormat4(Fold)
      implicit val flatMapFormat: RootJsonFormat[FlatMap] = jsonFormat2(FlatMap)
      implicit val filterFormat: RootJsonFormat[Filter] = jsonFormat2(Filter)
      implicit val groupByFormat: RootJsonFormat[GroupBy] = jsonFormat2(GroupBy)
      implicit val unionFormat: RootJsonFormat[Union] = jsonFormat2(Union)
      implicit val distinctFormat: RootJsonFormat[Distinct] = jsonFormat1(Distinct)
      implicit val fetchFormat: RootJsonFormat[Fetch] = jsonFormat1(Fetch)
      implicit val joinFormat: RootJsonFormat[Join] = jsonFormat4(Join)
      implicit val crossFormat: RootJsonFormat[Cross] = jsonFormat2(Cross)

      implicit val bindFormat: RootJsonFormat[Bind] = jsonFormat2(Bind)
      implicit val writeCsvFormat: RootJsonFormat[WriteCsv] = jsonFormat3(WriteCsv)
      implicit val writeTextFormat: RootJsonFormat[WriteText] = jsonFormat2(WriteText)
      implicit val writeParquet: RootJsonFormat[WriteParquet] = jsonFormat3(WriteParquet)

      private def mixinType[A <: DataFlow : JsonFormat : Meta](node: A): JsObject = {
        implicitly[JsonFormat[A]].write(node) match {
          case JsObject(fields) => JsObject(
            (("type" -> JsString(simpleName[A])) +: fields.toSeq).toMap
          )
          case x => serializationError(s"""Unable to mix type into non-object: $x""")
        }
      }

      def projectType(jsValue: JsValue): (JsObject, String) = jsValue match {
        case JsObject(fields) if fields.contains("type") =>
          val tpe = fields("type") match {
            case JsString(value) => value
            case x => deserializationError(s"""Unable to infer type. String expected, got `$x`""")
          }
          (JsObject(fields - "type"), tpe)
        case x => deserializationError(
          s"""Unable to project DataFlow variant. JSON-Object expected, got: `$x'"""
        )
      }

      def simpleName[A <: DataFlow : Meta]: String = implicitly[Meta[A]].ctag.runtimeClass.getSimpleName

      // TODO brain: this sucks
      override def read(json: JsValue): DataFlow = {
        val (o, tpe) = projectType(json)
        def mk[T <: DataFlow : JsonFormat]: T = o.convertTo[T]
        def is[T <: DataFlow : Meta]: Boolean = tpe == simpleName[T]
        if(is[ReadCsv]) mk[ReadCsv]
        else if(is[ReadText]) mk[ReadText]
        else if(is[ReadParquet]) mk[ReadParquet]
        else if(is[Ref]) mk[Ref]

        else if(is[Map]) mk[Map]
        else if(is[Fold]) mk[Fold]
        else if(is[FlatMap]) mk[FlatMap]
        else if(is[Filter]) mk[Filter]
        else if(is[GroupBy]) mk[GroupBy]
        else if(is[Union]) mk[Union]
        else if(is[Distinct]) mk[Distinct]
        else if(is[Fetch]) mk[Fetch]
        else if(is[Join]) mk[Join]
        else if(is[Cross]) mk[Cross]

        else if(is[Bind]) mk[Bind]
        else if(is[WriteCsv]) mk[WriteCsv]
        else if(is[WriteText]) mk[WriteText]
        else if(is[WriteParquet]) mk[WriteParquet]
        else deserializationError(s"""Cannot resolve type: $tpe""")
      }

      override def write(obj: DataFlow): JsValue = obj match {
        case rc: ReadCsv => mixinType(rc).toJson
        case rt: ReadText => mixinType(rt).toJson
        case rp: ReadParquet => mixinType(rp).toJson
        case r: Ref => mixinType(r).toJson
        case f: From => mixinType(f).toJson

        case m: Map => mixinType(m).toJson
        case f: Fold => mixinType(f).toJson
        case fm: FlatMap => mixinType(fm).toJson
        case f: Filter => mixinType(f).toJson
        case g: GroupBy => mixinType(g).toJson
        case u: Union => mixinType(u).toJson
        case d: Distinct => mixinType(d).toJson
        case f: Fetch => mixinType(f).toJson
        case j: Join => mixinType(j).toJson
        case c: Cross => mixinType(c).toJson

        case b: Bind => mixinType(b).toJson
        case wc: WriteCsv => mixinType(wc).toJson
        case wt: WriteText => mixinType(wt).toJson
        case wp: WriteParquet => mixinType(wp).toJson
      }
    }
  }
}
