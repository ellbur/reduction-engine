
package reductionengine.sugar

trait Idiom { val name: String }

/**
 * An idiomatic reconciliation is how we adapt an applicative junction to the idioms of its children.
 *
 * That is, how should
 *    3 + x
 * be computed when x has type Maybe Int?
 *
 * The answer, of course, is, "it depends", or, it should be left up to the user. In this example, of
 * course, there is only one thing you can do, namely (+ 3) <$> x or some equivalent thereof. But what
 * about this:
 *  x + y
 * where x, y are both Maybe Int. You might say, "Well, that should be (+) <$> x <*> y", but that's not
 * the only thing that is type-correct. You could also have it be
 *  map (\f -> map f y) (map (+) x) :: Maybe (Maybe Int)
 * which seems a little silly, but consider the Reader aka Hom functor, where this "overlapping" interpretation
 * corresponds to building a multiarity function out of two unary ones.
 */
case class IdiomaticReconciliation[+N](children: Seq[IdiomaticChild[N]], levels: Seq[ReconciliationLevel])

case class IdiomaticChild[+N](target: N, idiomStack: Seq[Idiom])

case class ReconciliationLevel(reconciler: Reconciler, involvedChildren: Seq[Int])

trait Reconciler {
  // TODO
}

case class ApplicativeReconciler(
  name: String, pure: SugarReplacement[Nothing], app: SugarReplacement[Nothing]) extends Reconciler
