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
package gui.backend

import api._
import api.backend._
import gui.GuiEnv

/**
 * Operators added by backend-agnostic transformations.
 *
 * Do not use those directly unless you want to hardcode physical execution aspects such as
 * join order and caching and you know exactly what you are doing.
 */
object GuiOps extends ComprehensionCombinators[GuiEnv] with Runtime[GuiEnv] {

  import Meta.Projections._
  import ScalaSeq.wrap

  //--------------------------------------------------------
  // ComprehensionCombinators
  //--------------------------------------------------------

  /** Naive `cross` node. */
  def cross[A, B](
    xs: DataBag[A], ys: DataBag[B]
  )(implicit A: Meta[A], B: Meta[B], env: GuiEnv): DataBag[(A, B)] =
    env.ops.cross(xs, ys)(A, B, env.rt)

  /** Naive `equiJoin` node. */
  def equiJoin[A, B, K](
    kx: A => K, ky: B => K)(xs: DataBag[A], ys: DataBag[B]
  )(implicit A: Meta[A], B: Meta[B], K: Meta[K], env: GuiEnv): DataBag[(A, B)] =
    env.ops.equiJoin(kx, ky)(xs, ys)(A, B, K, env.rt)

  //--------------------------------------------------------
  // Runtime
  //--------------------------------------------------------

  /** Implement the underlying logical semantics only (identity function). */
  def cache[A](xs: DataBag[A])(implicit A: Meta[A], env: GuiEnv): DataBag[A] =
    env.ops.cache(xs)(A, env.rt)

  /** Fuse a groupBy and a subsequent fold into a single operator. */
  def foldGroup[A, B, K](
    xs: DataBag[A], key: A => K, sng: A => B, uni: (B, B) => B
  )(implicit A: Meta[A], B: Meta[B], K: Meta[K], env: GuiEnv): DataBag[(K, B)] =
    env.ops.foldGroup(xs, key, sng, uni)(A, B, K, env.rt)
}
