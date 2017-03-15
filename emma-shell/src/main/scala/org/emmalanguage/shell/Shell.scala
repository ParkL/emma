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
package shell

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._

import scala.io.StdIn

object Shell extends App with LineModel {
  implicit val system = ActorSystem("emma-shell-as")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  // API:
  //
  // get / -> List[ExampleID]
  // post /ExampleId/run -> Session-ID
  // get /Session-ID -> Option[Graph] // may stall until available in "host" stalls when delivered unless done
  // put /Session-ID/continue -> Option[Graph] // if Session-Id is stalling, starts the next stage

  val route =
    pathSingleSlash {
      get {
        complete(s"Index of examples")
      }
    } ~
    path("yield") {
      get {
        complete(s"Hello World")
      }
    }

  val server = Http().bindAndHandle(route, "localhost", 8080)
  println("Running…")
  StdIn.readLine()
  server.flatMap(_.unbind()).onComplete(_ => system.terminate())
}