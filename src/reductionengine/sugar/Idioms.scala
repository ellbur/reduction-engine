
package reductionengine.sugar

trait Idioms { self: SugarNodes =>
  // A kind of idiom is different from an actual idiom because of alpha conversion :(
  case class KindOfIdiom(name: String, pure: RNode, app: RNode) {
    override def toString = name
  }

  case class Idiom(kind: KindOfIdiom, name: String) {
    override def toString = s"$kind($name)"
    def toLogic = logic.Idiom(this, toNode(kind.pure), toNode(kind.app))
  }
}
