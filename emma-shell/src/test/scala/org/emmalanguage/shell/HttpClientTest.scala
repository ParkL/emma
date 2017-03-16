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

import org.emmalanguage.shell.util.GraphEasySupport

import akka.actor.ActorSystem
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers
import spray.json._

import scala.concurrent.Await
import scala.concurrent.Future

class HttpClientTest // "End-To-End"
  extends TestKit(ActorSystem("HttpTestSystem"))
  with ImplicitSender
  with FreeSpecLike with Matchers
  with BeforeAndAfterAll
  with LineModel {
  import gui.Model._
  import scala.concurrent.duration._

  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  """Receive a DataFlow from the server""" in {
    val f: Future[(DataFlow, Http.ServerBinding)] = for {
      binding <- Shell.bind(port = 0)
      addr = binding.localAddress
      resp <- Http().singleRequest(HttpRequest(uri = s"http:/${addr.toString}/test"))
      body <- resp.entity.dataBytes.runFold(ByteString(""))(_ ++ _)
    } yield (body.utf8String.parseJson.convertTo[DataFlow], binding)
    f.onFailure {
      case err: Throwable => fail(err)
    }
    val (flow, binding) = Await.result(f, 10.seconds)
    val graph = flow.mkGraph()
    for { print <- GraphEasySupport.printer() } print(graph)
    Await.result(binding.unbind, 10.seconds)
  }
}
