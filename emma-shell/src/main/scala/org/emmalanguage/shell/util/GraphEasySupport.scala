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

object GraphEasySupport {
  import gui.Model._
//  def mkGraphEasy(dataFlow: DataFlow): Seq[String] = {
//
//
//  }

//    def dwellTimeToString(dwellDuration: DwellDuration) = {
//      val from :: to :: Nil = List(dwellDuration.from, dwellDuration.to)
//        .map(_.toString.replace(" ",""))
//      s"""$from-$to"""
//    }
//    val sx = for {
//      Transition(from, to, dwellDuration) <- dataFlow.transitions
//    } yield {
//      dwellDuration match {
//        case Some(dwellTime) => s"[ ${from.title} ] - ${dwellTimeToString(dwellTime)} -> [ ${to.title } ]"
//        case None => s"[ ${from.title} ] -> [ ${to.title} ]"
//      }
//    }
//    sx.toSeq
//  }

//  def printGraph(graph: Graph): Int = {
//    val file = java.io.File.createTempFile("graph", "easy")
//    val bw = new BufferedWriter(new FileWriter(file))
//    bw.write(mkGraphEasy(graph).mkString("\n"))
//    bw.close()
//    val path = file.getAbsolutePath
//    val result = s"""graph-easy $path""".!
//    s"""rm $path""".! // cleanup
//    result
//  }

}
