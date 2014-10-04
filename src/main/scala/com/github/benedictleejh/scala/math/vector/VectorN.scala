package com.github.benedictleejh.scala.math.vector

import scala.math.sqrt

case class VectorN[T](private val components: T*)(implicit n: Numeric[T]) {
  class IncompatibleDimensionException extends Exception

  private def square(x: T) = n.toDouble(x) * n.toDouble(x)

  def apply(index: Int): T = components(index)

  override def toString: String = s"<${components.mkString(", ")}>"

  def length: Double = sqrt(components.map(square(_)).reduce(_+_))

  def dimension: Int = components.length

  def unary_- = new VectorN(components.map(n.negate(_)): _*)

  def +[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = {
    if (this.dimension != that.dimension) throw new IncompatibleDimensionException
    new VectorN[C](components.zip(that.components).map { case (n1,n2) => nl.plus(n1,n2) }: _*)
  }
  def -[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = this + -that

  def *[B,C](s: B)(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = new VectorN[C](components.map(nl.times(_, s)): _*)
  def *:[B, C](s: B)(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = this * s

  def /[B, C](s: B)(implicit nl: NumberLike[T, B, C], nb: Numeric[B], nc: Numeric[C]): VectorN[C] = new VectorN[C](components.map(nl.divide(_, s)): _*)

  def normalise[C](implicit nl: NumberLike[T, Double, C], nc: Numeric[C]): VectorN[C] = this / length

  def dot[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): C = {
    if (this.dimension != that.dimension) throw new IncompatibleDimensionException
    components.zip(that.components).map({case (n1, n2) => nl.times(n1, n2)}).reduce(nc.plus(_, _))
  }
  def ⋅[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): C = this dot that

  def cross[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = {
    val is3D = this.dimension == 3 && that.dimension == 3
    val is7D = this.dimension == 7 && that.dimension == 7
    if (!is3D || !is7D) throw new IncompatibleDimensionException

    if (is3D) {
      new VectorN[C](
        nc.minus(nl.times(this(1), that(2)), nl.times(this(2), that(1))),
        nc.minus(nl.times(this(2), that(0)), nl.times(this(0), that(2))),
        nc.minus(nl.times(this(0), that(1)), nl.times(this(1), that(0)))
      )
    }
    else {
      import nc.mkNumericOps
      val x1 = this(0)
      val x2 = this(1)
      val x3 = this(2)
      val x4 = this(3)
      val x5 = this(4)
      val x6 = this(5)
      val x7 = this(6)

      val y1 = that(0)
      val y2 = that(1)
      val y3 = that(2)
      val y4 = that(3)
      val y5 = that(4)
      val y6 = that(5)
      val y7 = that(6)


      new VectorN[C](
        nl.times(x2, y4) - nl.times(x4, y2) + nl.times(x3, y7) - nl.times(x7, y3) + nl.times(x5, y6) - nl.times(x6, y5),
        nl.times(x3, y5) - nl.times(x5, y3) + nl.times(x4, y1) - nl.times(x1, y4) + nl.times(x6, y7) - nl.times(x7, y6),
        nl.times(x4, y6) - nl.times(x6, y4) + nl.times(x5, y2) - nl.times(x2, y5) + nl.times(x7, y1) - nl.times(x1, y7),
        nl.times(x5, y7) - nl.times(x7, y5) + nl.times(x6, y3) - nl.times(x3, y6) + nl.times(x1, y2) - nl.times(x2, y1),
        nl.times(x6, y1) - nl.times(x1, y6) + nl.times(x7, y4) - nl.times(x4, y7) + nl.times(x2, y3) - nl.times(x3, y2),
        nl.times(x7, y2) - nl.times(x2, y7) + nl.times(x1, y5) - nl.times(x5, y1) + nl.times(x3, y4) - nl.times(x4, y3),
        nl.times(x1, y3) - nl.times(x3, y1) + nl.times(x2, y6) - nl.times(x6, y2) + nl.times(x4, y5) - nl.times(x5, y4)
      )
    }
  }

  def ×[B, C](that: VectorN[B])(implicit nl: NumberLike[T, B, C], nc: Numeric[C]): VectorN[C] = this cross that
}
