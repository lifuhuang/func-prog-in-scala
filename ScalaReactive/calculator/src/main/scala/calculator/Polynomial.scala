package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val x1 = (-b() - math.sqrt(delta())) / (2 * a())
      val x2 = (-b() + math.sqrt(delta())) / (2 * a())
      Set(x1, x2).filterNot(_.isNaN)
    })
  }
}
