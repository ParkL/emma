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

object Shell extends LineModel {
  import gui.Model._
  // API:
  //
  // get / -> List[ExampleID]
  // post /ExampleId/run -> Session-ID
  // get /Session-ID -> Option[Graph] // may stall until available in "host" stalls when delivered unless done
  // put /Session-ID/continue -> Option[Graph] // if Session-Id is stalling, starts the next stage

  // TODO brain type ascription is necessary
  val flow: DataFlow =  Map("map", ReadText("textPath"))

  val route =
    path("test") {
      get {
        complete(flow)
      }
    }

  def bind(interface: String = "localhost", port: Int = 8080)(implicit system: ActorSystem) = {
    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(route, interface, port)
  }

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("emma-shell-as")
    implicit val executionContext = system.dispatcher
    for { binding <- bind() } {
      println(s"Server running at ${binding.localAddress}. Press any key to quit.")
      StdIn.readLine()
      binding.unbind().onComplete { _ => system.terminate() }
    }
  }
}
