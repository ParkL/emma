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
package compiler.shell

import api._
import api.Meta.Projections._
import org.emmalanguage.compiler.RuntimeCompiler
import compiler.ir.ComprehensionSyntax._
import io.csv._

import akka.actor.ActorSystem

import scala.reflect.internal.util

object Test extends App {
  import akka.stream._
  import akka.stream.scaladsl._

  implicit val system = ActorSystem("test-system")
  implicit val ec = system.dispatcher
  implicit val materializer = ActorMaterializer()

  val compiler = new RuntimeCompiler()

  import compiler._

  // Source(1 to 10).runWith(Sink.foreach(println))

//  val sourceQueue = akka.stream.scaladsl.Source.queue[String](5, akka.stream.OverflowStrategy.backpressure)
//  val q = sourceQueue.toMat(Sink.foreach(println)) { (queue ,_) =>
//    val liftPipeline: u.Expr[Any] => u.Tree =
//      compiler.pipeline(typeCheck = true)(
//        LibSupport.expand,
//        GraphTools.mkJsonGraphAsString(s"post-expand")(s => queue.offer(s)),
//        Core.lift,
//        GraphTools.mkJsonGraphAsString(s"post-lift")(s => queue.offer(s))
//      ).compose(_.tree)
//    liftPipeline(coreExpr)
//  }

  val anfPipeline: u.Expr[Any] => u.Tree =
    compiler.pipeline(typeCheck = true)(
      Core.anf
    ).compose(_.tree)

  val liftPipeline: u.Expr[Any] => u.Tree =
    compiler.pipeline(typeCheck = true)(
      LibSupport.expand,
      Core.lift
    ).compose(_.tree)

  final case class Edge[V](src: V, dst: V)

  val input: String = null
  val output: String = null
  val csv = CSV()

  val coreExpr = anfPipeline(u.reify {
    // read in a directed graph
    val input = this.input
    val csv$1 = this.csv
    val readCSV = DataBag.readCSV[Edge[Int]](input, csv$1)
    val paths$1 = readCSV.distinct
    val count$1 = paths$1.size
    val added$1 = 0L

    def doWhile$1(added$3: Long, count$3: Long, paths$3: DataBag[Edge[Int]]): Unit = {

      val closure = comprehension[Edge[Int], DataBag] {
        val e1 = generator[Edge[Int], DataBag](paths$3)
        val e2 = generator[Edge[Int], DataBag](paths$3)
        guard(e1.dst == e2.src)
        head {
          Edge(e1.src, e2.dst)
        }
      }

      val paths$2 = (paths$3 union closure).distinct
      val added$2 = paths$2.size - count$3
      val count$2 = paths$2.size
      val isReady = added$2 > 0

      def suffix$1(): Unit = {
        val closure = paths$2
        val output = this.output
        val csv$2 = this.csv
        closure.writeCSV(output, csv$2)
      }

      if (isReady) doWhile$1(added$2, count$2, paths$2)
      else suffix$1()
    }

    doWhile$1(added$1, count$1, paths$1)
  })


}
