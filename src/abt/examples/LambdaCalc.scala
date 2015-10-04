package abt
package examples

import ABTs._
import scalaz.Functor
import scalaz.Foldable
import scalaz.Monoid

/**
  * A Curry-style typed lambda calculus, implemented using ABTs.
  */
object LambdaCalc {
  trait SimpleType
  case object Base extends SimpleType
  case class Arrow(t1: SimpleType, t2: SimpleType) extends SimpleType

  protected sealed trait _TLambda[T]
  private case class _Lam[T](t: T) extends _TLambda[T]
  private case class _App[T](t1: T, t2: T) extends _TLambda[T]
  private case class _Let[T](t1: T, t2: T) extends _TLambda[T]
  private case class _Annot[T](t: T, tp: SimpleType) extends _TLambda[T]

  implicit val lambdaSig: Functor[_TLambda] with Foldable[_TLambda] =
    new Functor[_TLambda] with Foldable[_TLambda] with Foldable.FromFoldMap[_TLambda] {
      def map[A, B](fa: _TLambda[A])(f: A => B): _TLambda[B] = fa match {
        case _Lam(t) => _Lam(f(t))
        case _Annot(t, tp) => _Annot(f(t), tp)
        case _App(t1, t2) => _App(f(t1), f(t2))
        case _Let(t1, t2) => _Let(f(t1), f(t2))
      }

      def foldMap[A,B](fa: _TLambda[A])(f: A => B)(implicit F: Monoid[B]): B =
        fa match {
          case _Lam(t) => f(t)
          case _Annot(t, _) => f(t)
          case _App(t1, t2) => F.append(f(t1), f(t2))
          case _Let(t1, t2) => F.append(f(t1), f(t2))
        }
    }

  val lambdaAbt: IAbt[_TLambda] = new Abt

  //Needed reexports
  type Term = lambdaAbt.Term
  val Var = lambdaAbt.Var
  type Name = ABTs.Name
  val Name = ABTs.Name
  implicit val TermOps = lambdaAbt.TermOps _

  //
  import lambdaAbt._
  //Smart constructors/extractors
  object Lam {
    def apply(name: Name, body: Term): Term =
      _Tm(_Lam(_Abs(name, body)))
    def unapply(t: Term): Option[(Name, Term)] = t match {
      case _Tm(_Lam(_Abs(name, body))) => Some((name, body))
      case _ => None
    }
  }

  object App {
    def apply(f: Term, arg: Term): Term =
      _Tm(_App(f, arg))
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case _Tm(_App(f, arg)) => Some((f, arg))
      case _ => None
    }
  }

  object Annot {
    def apply(t: Term, tp: SimpleType): Term =
      _Tm(_Annot(t, tp))
    def unapply(t: Term): Option[(Term, SimpleType)] = t match {
      case _Tm(_Annot(t, tp)) => Some((t, tp))
      case _ => None
    }
  }

  object Let {
    def apply(name: Name, t1: Term, t2: Term): Term =
      _Tm(_Let(t1, _Abs(name, t2)))
    def unapply(t: Term): Option[(Name, Term, Term)] = t match {
      case _Tm(_Let(t1, _Abs(name, t2))) => Some((name, t1, t2))
      case _ => None
    }
  }
}
