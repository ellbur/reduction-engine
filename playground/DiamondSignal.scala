
object DiamondSignal {
  import reactive._

  def main(args: Array[String]) {
    implicit val obs = new Observing {}

    val x = Var[Int](0)
    val y = Var[Int](0)
    val z = (x zip y) map {
      case (x, y) => x + y
    }
    val w = (x zip y) map {
      case (x, y) => x - y
    }
    val t = (z zip w) map {
      case (z, w) =>
        println("Calculating t")
        z * w
    }

    t foreach (println(_))

    x() = 1
  }
}

