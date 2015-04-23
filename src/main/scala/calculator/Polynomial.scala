package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()
      val aVal = a()
      val cVal = c()

      (bVal * bVal) - (4 * aVal * cVal)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {

      val bVal = b()
      val aVal = a()

      delta() match {
        case d if d < 0 => Set()
        case d if d == 0 => Set((-bVal) / (2 * aVal))
        case d => {

          Set(
            (-bVal + math.sqrt(d)) / (2 * aVal),
            (-bVal - math.sqrt(d)) / (2 * aVal)
          )
        }
      }
    }
  }
}
