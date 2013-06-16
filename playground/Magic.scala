
import java.io.{FileWriter, File}
import scala.collection.mutable

object Magic extends App {
  /*
  // Technically only some idioms support `bind`.
  case class IdiomKind(name: String, pure: Tree, app: Tree, bind: Tree) {
    override lazy val toString = name
  }

  case class Idiom(kind: IdiomKind, name: String) {
    override lazy val toString = s"${kind.name}($name)"
  }

  sealed trait Tree {
    def apply(arg: Tree) = App(this, arg)
  }
  case class App(car: Tree, cdr: Tree) extends Tree
  case object M extends Tree
  case object I extends Tree
  case class Pure(idiom: Idiom) extends Tree {
    override lazy val toString = s"K($idiom,$name)"
  }
  case class AntiPure(idiom: Idiom) extends Tree {
    override lazy val toString = s"ʞ($idiom,$name)"
  }
  case class Var(name: String) extends Tree

  case class OpenTerm(idioms: Set[Idiom], is: Tree)

  def v(name: String) = AntiPure("V", name)(I)
  def l(name: String) = Pure("V", name)

  def magicify(t: Tree): Tree = t match {
    case App(car, cdr) => M(magicify(car))(magicify(cdr))
    case Pure(Idiom(name, pure, app, bind)) => Pure(Idiom(name, magicify(pure), magicify(app), magicify(bind)))
    case AntiPure(Idiom(name, pure, app, bind)) => AntiPure(Idiom(name, magicify(pure), magicify(app), magicify(bind)))
    case o => OpenTerm(Set(), o)
  }

  def reduceOnce(t: Tree): Option[Tree] =
    reductionAt(t) match {
      case Some(r) => Some(r)
      case None =>
        t match {
          case App(car, cdr) =>
            reduceOnce(car) match {
              case Some(r) => Some(r(cdr))
              case None =>
                reduceOnce(cdr) match {
                  case Some(r) => Some(car(r))
                  case None => None
                }
            }
          case _ => None
        }
    }

  def reductionAt(t: Tree) = t match {
    case App(I, x) => Some(x)
    case App(App(M, AntiPure(idiom)), OpenTerm(idioms, f)) =>
      if (idioms.contains(idiom)) {
        val bind = idiom.kind.bind
        val idiomsY = idioms - idiom
        M(AntiPure(idiom))(OpenTerm(idiomsY, bind(OpenTerm(idiomsY, f))))
      }
      else {
        OpenTerm(idioms + idiom, f)
      }
    case App(App(M, o1 @ OpenTerm(idiomsX, f)), o2 @ OpenTerm(idiomsY, g)) =>
      OpenTerm(idiomsX ++ idiomsY, App(o1, o2))
    case App(App(M, Pure(idiom)), OpenTerm())
  }

  def allReductions(t: Tree): List[Tree] = t :: (reduceOnce(t) match {
    case Some(next) => allReductions(next)
    case None => Nil
  })

  /**
   * Convert to graphviz graph.
   */
  def toGraph(tree: Tree): String = {
    case class Node(name: String, shape: String, label: String)

    val nodes = mutable.ArrayBuffer[Node]()
    val edges = mutable.ArrayBuffer[(String,String)]()
    var nameCounter: Int = 0

    def nextName() = {
      nameCounter += 1
      s"n$nameCounter"
    }

    def handle(tree: Tree): String = {
      val name = nextName()
      val (shape, label) = tree match {
        case App(_, _) => ("point", "")
        case _ => ("none", tree.toString)
      }
      nodes += Node(name, shape, label)

      tree match {
        case App(car, cdr) =>
          val carName = handle(car)
          val cdrName = handle(cdr)

          edges += ((name, carName))
          edges += ((name, cdrName))
        case _ =>
      }

      name
    }
    handle(tree)

    (
         "digraph yo {\n"
      ++ (nodes map (n => s"""${n.name} [shape="${n.shape}",label="${n.label}"];\n""") mkString "")
      ++ ((edges map {case (a, b) => s"$a -> $b;\n"}) mkString "")
      ++ "}\n"
    )
  }

  def see(t: Tree) {
    val dotfile = File.createTempFile("hiii", ".dot")
    val w = new FileWriter(dotfile)
    w.write(toGraph(t))
    w.flush()
    w.close()

    val pngFile = File.createTempFile("hiii", ".png")
    Runtime.getRuntime.exec(Array("dot", "-Tpng", s"-o${pngFile.getAbsolutePath}", dotfile.getAbsolutePath)).waitFor()

    Runtime.getRuntime.exec(Array("eog", "-n", pngFile.getAbsolutePath)).waitFor()
  }

  def seeReductions(t: Tree) {
    for (step <- allReductions(t)) {
      see(step)
    }
  }

  val starting =
    l("x") (
      l("y")(
        v("x")(v("y"))
      )
    )

  val magicked = magicify(starting)

  seeReductions(I(I(Var("x"))))
  */
}
