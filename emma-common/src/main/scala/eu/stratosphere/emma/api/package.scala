package eu.stratosphere.emma

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URI

import eu.stratosphere.emma.api.model.Identity
import eu.stratosphere.emma.macros.{Folds, ConvertorsMacros}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}

/**
 * This is e playground specification of e function-oriented algebra for the Emma language.
 */
package object api {

  import scala.language.experimental.macros

  lazy val hdfs = FileSystem.get(new Configuration())

  // -----------------------------------------------------
  // language primitives
  // -----------------------------------------------------

  /**
   * Reads a DataBag from the given `url` location using the provided `format`.
   *
   * @param path The URI location of the constructed DataBag.
   * @param format The format used to read DataBag elements.
   * @tparam A The type of the DataBag elements.
   * @return A DataBag read from the given `url`.
   */
  def read[A](path: String, format: InputFormat[A]): DataBag[A] = DataBag({
    // construct a LocalFS or HDFS input stream for the given path
    val is = {
      val uri = new URI(path)
      if (uri.getScheme == "hdfs") {
        hdfs.open(new Path(uri))
      } else {
        new FileInputStream(new File(path))
      }
    }

    // read the contents of the input stream as Seq[A] and use it to construct the DataBag
    format.read(is)
  })

  /**
   * Writes a DataBag `in` to a `url` using the provided `format`.
   *
   * @param path The URI location where the DataBag should be written.
   * @param format The format used to write DataBag elements.
   * @param in The set to be written.
   * @tparam A The type of the DataBag elements.
   * @return Unit
   */
  def write[A](path: String, format: OutputFormat[A])(in: DataBag[A]): Unit = {
    // construct a LocalFS or HDFS output stream for the given path
    val os = {
      val uri = new URI(path)
      if (uri.getScheme == "hdfs") {
        hdfs.create(new Path(uri), true)
      } else {
        new FileOutputStream(new File(path))
      }
    }

    // write the elements of this DataBag elements into the constructed output stream
    format.write(in, os)
  }

  /**
   * Converts an immutable DataBag into a stateful version which permits in-place pointwise updates.
   *
   * @param bag The bag to be used to initialize the state.
   * @tparam A The type of the state elements. Must be a subtype of Identity[K]
   * @tparam K The key to be used to index the elements.
   * @return A Stateful.Bag instance initialized with the elements of the given `bag`.
   */
  def stateful[A <: Identity[K], K](bag: DataBag[A]): Stateful.Bag[A, K] = new Stateful.Bag[A, K](bag)

  // ---------------------------------------------------
  // Algorithm
  // ---------------------------------------------------

  /**
   * Wrapper around an algorithm (generated by macros).
   *
   * @tparam T The return type of the algorithm.
   */
  trait Algorithm[T] {
    def run(engine: runtime.Engine): T
  }

  // -----------------------------------------------------
  // limits
  // -----------------------------------------------------

  trait Limits[T] {
    val min: T
    val max: T
  }

  implicit object ByteLimits extends Limits[Byte] {
    val min = Byte.MinValue
    val max = Byte.MaxValue
  }

  implicit object IntLimits extends Limits[Int] {
    val min = Int.MinValue
    val max = Int.MaxValue
  }

  implicit object LongLimits extends Limits[Long] {
    val min = Long.MinValue
    val max = Long.MaxValue
  }

  implicit object CharLimits extends Limits[Char] {
    val min = Char.MinValue
    val max = Char.MaxValue
  }

  implicit object FloatLimits extends Limits[Float] {
    val min = Float.MinValue
    val max = Float.MaxValue
  }

  implicit object DoubleLimits extends Limits[Double] {
    val min = Double.MinValue
    val max = Double.MaxValue
  }

  // -----------------------------------------------------
  // convertors
  // -----------------------------------------------------

  /**
   * Extend the [[DataBag]] type with methods from [[Folds]] via the "pimp my library" pattern.
   * This is a value class, which means that in most cases, the allocation of an instance can be
   * avoided when using the defined methods.
   * @param self the actual [[DataBag]] instance
   * @tparam E the type of elements to fold over
   */
  implicit class DataBagWithFolds[+E](val self: DataBag[E]) extends AnyVal with Folds[E] {
    def fold[R](z: R, s: E => R, p: (R, R) => R) = self.fold[R](z, s, p)
  }

  implicit def materializeCSVConvertors[T]: CSVConvertors[T] = macro ConvertorsMacros.materializeCSVConvertorsImpl[T]
}