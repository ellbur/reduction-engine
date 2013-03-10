import collection.immutable.Nil

object Misc {
  def firstSucceeding[A,B](arg: A, pfs: List[PartialFunction[A,B]]): B = pfs match {
    case Nil =>
      throw new MatchError()
    case pf :: pfs =>
      try {
        pf(arg)
      }
      catch {
        case _: MatchError =>
          firstSucceeding(arg, pfs)
      }
  }
}
