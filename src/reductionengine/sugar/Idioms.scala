
package reductionengine.sugar

trait Idioms { self: SugarNodes =>
  // A kind of idiom is different from an actual idiom because of alpha conversion :(
  case class KindOfIdiom(name: String, pure: RNode, app: RNode) {
    override def toString = name
  }

  case class Idiom(kind: KindOfIdiom, name: String) {
    override def toString = s"$kind($name)"
    def toLogic = logic.Idiom(this, kind.pure.toNode, kind.app.toNode)
  }

  object standardCombinators {
    import self.{NewNode => NN}

    val K = NN(ApicalOperator(BasicOperator(logic.K(1, Seq(false))), Seq()))
    val S = NN(ApicalOperator(BasicOperator(logic.S(1)), Seq()))
    val I = NN(ApicalOperator(BasicOperator(logic.I), Seq()))
  }

  object standardIdiomKinds {
    import self.{NewNode => NN}
    import standardCombinators._

    val lambda = KindOfIdiom("Var", K, S)
  }
}
