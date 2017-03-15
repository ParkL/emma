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
package shell.util

import java.io.BufferedWriter
import java.io.FileWriter
import scala.sys.process._

object GraphEasySupport {
  import gui.Model._

  type Edges = Set[(NodeId, NodeId)]
  type Printer = Option[Edges => Int]

  lazy val hasGraphEasy: Boolean = "which graph-easy".! == 0

  def printer(): Printer = if(hasGraphEasy) Some(printGraph _) else None

  private def mkGraphEasy(edges: Set[(NodeId, NodeId)]): Seq[String] = {
    def renderEdge(edge: (NodeId, NodeId)): String = {
      val (from, to) = edge
      s"[$from] -> [$to]"
    }
    edges.map(renderEdge).toSeq
  }

  private def printGraph(edges: Set[(NodeId, NodeId)]): Int = {
    val file = java.io.File.createTempFile("graph", "easy")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(mkGraphEasy(edges).mkString("\n"))
    bw.close()
    val path = file.getAbsolutePath
    val result = s"""graph-easy $path""".!
    s"""rm $path""".! // cleanup
    result
  }
}
