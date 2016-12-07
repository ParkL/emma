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
import api.Meta._
import api.Meta.Projections._
import org.emmalanguage.compiler.RuntimeCompiler
import compiler.ir.ComprehensionSyntax._
import io.csv._
import org.emmalanguage.examples.graphs.TransitiveClosure
import org.emmalanguage.examples.graphs.model.Edge

import akka.actor.ActorSystem

import scala.concurrent.Await
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

  def interleave(pipeline: Seq[u.Tree => u.Tree], insert: String => u.Tree => u.Tree): Seq[u.Tree => u.Tree] =
    pipeline.zipWithIndex.flatMap { case (tf, i) => Seq(tf, insert(s"stage-$i"))  }

  val sourceQueue = akka.stream.scaladsl.Source.queue[String](5, akka.stream.OverflowStrategy.backpressure)

  val queue = sourceQueue.to(Sink.foreach(println)).run()

  // IntelliJ Covariance / Contravariance Highlight error with String => Unit
  val applyName = GraphTools.mkJsonGraphAsString(enqueue) _

  val enqueue: GraphTools.EnqueueEffect[String] = queue.offer

  val liftStages = Seq(LibSupport.expand, Core.lift)
  val liftInterleaved = interleave(liftStages, applyName)
  val lift = compiler.pipeline(typeCheck = true)(liftInterleaved:_*)
  val liftPipeline: u.Expr[Any] => u.Tree = lift.compose(_.tree)

  val input: String = null
  val output: String = null
  val csv = CSV()
  implicit val edgeCSVConverter = CSVConverter[Edge[Int]]

  val sourceExpr = liftPipeline(u.reify {
    // read an initial collection of edges
    val edges = DataBag.readCSV[Edge[Int]](input, csv)
    // compute the transitive closure of the edges
    val closure = TransitiveClosure(edges)
    // write the results into a CSV file
    closure.writeCSV(output, csv)
  })

  import scala.concurrent.duration._
  Await.result(system.terminate(), 5.seconds)

}
