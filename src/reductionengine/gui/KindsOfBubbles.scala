
package reductionengine.gui

trait KindsOfBubbles { this: Editor =>
  case class App(
                  var car: BubbleContainer,
                  var cdr: BubbleContainer,
                  var x: Int,
                  var y: Int) extends Bubble with AppRender
  {
    def children = Seq(car, cdr)
  }
  object App {
    def ~(a: Replacement, b: Replacement) = NewBubble((ch,x,y) => App(ch(0), ch(1), x, y), Seq(a, b))
  }

  case class IntLiteral(
                         var n: Int,
                         var x: Int,
                         var y: Int) extends Bubble with IntLiteralRender
  {
    def children = Seq()
  }

  case class Plus(var x: Int, var y: Int) extends Bubble with PlusRender
  {
    def children = Seq()
  }
  object Plus {
    def ~ = NewBubble((ch, x, y) => Plus(x, y), Seq())
  }

  case class Root(var is: BubbleContainer, var x: Int, var y: Int) extends Bubble with RootRender
  {
    def children = Seq(is)
  }

  case class Mystery(var x: Int, var y: Int) extends Bubble with MysteryRender
  {
    def children = Seq()
  }
  object Mystery {
    def ~ = NewBubble((ch, x, y) => Mystery(x, y), Seq())
  }
}
