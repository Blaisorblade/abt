package abt
package examples

import ABTs._

object BidirTypechecking {
  import LambdaCalc._

  type Ctx = Map[Name, SimpleType]

  def isSynth: Term => Boolean = {
    case Lam(_, _) => false
    case Let(_, _, _) => false
    case _ => true
  }
  def isCheck(bt: Term): Boolean = !isSynth(bt)

  def fail(msg: String) =
    throw new IllegalArgumentException(msg)

  def synth(ctx: Ctx, e: Term): SimpleType = {
    e match {
      case Var(x) =>
        ctx get x getOrElse fail("unbound variable")
      case Annot(e, tp) =>
        check(ctx, e, tp)
        tp
      case App(f, e) =>
        synth(ctx, f) match {
          case Arrow(s, t) =>
            check(ctx, e, s)
            t
          case _ => fail("Applying a non-function!")
        }
      case _ if isCheck(e) =>
        fail("Cannot synthesize type for checking term")
      case _ =>
        fail("Unexpected term")
    }
  }

  def check(ctx: Ctx, e: Term, tp: SimpleType): Unit = {
    (e, tp) match {
      //Lambda
      case (Lam(x, e1), Arrow(tp1, tp2)) =>
        check(ctx updated (x, tp1), e1, tp2)
      case (Lam(_, _), _) =>
        fail("Expected arrow type")
      //Let
      case (Let(x, e1, e2), _) =>
        val tp1 = synth(ctx, e1)
        check(ctx updated (x, tp1), e2, tp)
      case _ if isSynth(e) =>
        if (tp == synth(ctx, e))
          ()
        else fail("Type mismatch")
      case _ =>
        fail("Unexpected term")
    }
  }
}
