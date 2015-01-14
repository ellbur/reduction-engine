
package reductionengine.sugar

trait Idioms { self: SugarNodes =>
  // A kind of idiom is different from an actual idiom because of alpha conversion :(
  case class KindOfIdiom(name: String, pure: RNode, app: RNode, join: Option[RNode], predict: Option[RNode]) {
    override def toString = name
  }

  case class Idiom(kind: KindOfIdiom, name: String) {
    import logic.{AlreadyThere => AT}

    override def toString = s"$kind($name)"
    def toLogic = logic.Idiom(this, AT(kind.pure), AT(kind.app), kind.join map (AT(_)), kind.predict map (AT(_)))
  }

  object standardCombinators {
    import self.{NewNode => NN}
    import standardIdiomKinds._

    val K = NN(ApicalOperator(BasicOperator(logic.K_), Seq()))
    val S = NN(ApicalOperator(BasicOperator(logic.S(1)), Seq()))
    val I = NN(ApicalOperator(BasicOperator(logic.I), Seq()))
    val Y = NN(ApicalOperator(BasicOperator(logic.Y), Seq()))
    val J = NN(ApicalOperator(BasicOperator(logic.J), Seq()))
    val Pr = NN(ApicalOperator(BasicOperator(logic.Pr), Seq()))
    val M = NN(ApicalOperator(BasicOperator(logic.Eliminator), Seq()))

    object vars {
      lazy val x = Idiom(lambda, "x")
      lazy val xs = Idiom(lambda, "xs")
      lazy val y = Idiom(lambda, "y")
      lazy val ys = Idiom(lambda, "ys")
      lazy val a = Idiom(lambda, "a")
      lazy val b = Idiom(lambda, "b")
      lazy val f = Idiom(lambda, "f")
      lazy val rec = Idiom(lambda, "rec")

      lazy val _x = NN(ApicalOperator(AntiPureOp(x), Seq(I)))
      lazy val _xs = NN(ApicalOperator(AntiPureOp(xs), Seq(I)))
      lazy val _y = NN(ApicalOperator(AntiPureOp(y), Seq(I)))
      lazy val _ys = NN(ApicalOperator(AntiPureOp(ys), Seq(I)))
      lazy val _a = NN(ApicalOperator(AntiPureOp(a), Seq(I)))
      lazy val _b = NN(ApicalOperator(AntiPureOp(b), Seq(I)))
      lazy val _f = NN(ApicalOperator(AntiPureOp(f), Seq(I)))
      lazy val _rec = NN(ApicalOperator(AntiPureOp(rec), Seq(I)))
    }
    import vars._

    def knownAs(n: RNode, name: String): RNode =
      NN(ApicalOperator(NodeOp(name, n, 0), Seq()))

    lazy val nil = NN(ApicalOperator(BasicOperator(logic.PairListNil), Seq()))
    lazy val cons = NN(ApicalOperator(BasicOperator(logic.PairListCons), Seq()))
    lazy val listP = knownAs(NN(Pure(x, cons(_x)(nil))), "P[]")
    lazy val listMap = knownAs(Y(NN(Pure(rec, NN(Pure(a,NN(Pure(f,
      M(_a)(nil)(NN(Pure(x, NN(Pure(xs,
        cons(_f(_x))(_rec(_xs)(_f))
      )))))
    ))))))), "map[]")
    lazy val listConcat = knownAs(Y(NN(Pure(rec, NN(Pure(a, NN(Pure(b,
      M(_a)(_b)(NN(Pure(x, NN(Pure(xs,
        cons(_x)(_rec(_xs)(_b))
      )))))
    ))))))), "++")
    lazy val listJoin = knownAs(Y(NN(Pure(rec, NN(Pure(a,
      M(_a)(nil)(NN(Pure(x, NN(Pure(xs,
        listConcat(_x)(_rec(_xs))
      )))))
    ))))), "join[]")
    lazy val listA = knownAs(NN(Pure(a, NN(Pure(b,
      listJoin(listMap(_a)(NN(Pure(x,
        listMap(_b)(NN(Pure(y,
          _x(_y)
        )))
      ))))
    )))), "A[]")

    lazy val nothing = NN(ApicalOperator(BasicOperator(logic.MaybeNothing), Seq()))
    lazy val just = NN(ApicalOperator(BasicOperator(logic.MaybeJust), Seq()))
    lazy val maybeP = just
    lazy val maybeA = knownAs(NN(Pure(a,NN(Pure(b,
      M(_a)(nothing)(NN(Pure(x,
        M(_b)(nothing)(NN(Pure(y,
          just(_x(_y))
        )))
      )))
    )))), "A[Maybe]")
  }

  object standardIdiomKinds {
    import self.{NewNode => NN}
    import standardCombinators._

    lazy val lambda = KindOfIdiom("λ", K, S, Some(J), Some(Pr))
    lazy val list = KindOfIdiom("[]", listP, listA, Some(listJoin), None)

    // TODO: Maybe join
    lazy val maybe = KindOfIdiom("Maybe", maybeP, maybeA, None, None)
  }
}
