package abt
package examples

import ABTs._
import scalaz.Functor
import scalaz.Foldable
import scalaz.Monoid

/**
 * @author pgiarrusso
 */
object MiniMLTypeSyn {
  protected sealed trait _Type[T]
  private case class _Base[T]() extends _Type[T]
  private case class _Arrow[T](t1: T, t2: T) extends _Type[T]

  private case class _Poly[T](t: T) extends _Type[T]

  implicit val typeSig: Functor[_Type] with Foldable[_Type] =
    new Functor[_Type] with Foldable[_Type] with Foldable.FromFoldMap[_Type] {
      def map[A, B](fa: _Type[A])(f: A => B): _Type[B] =
        fa match {
          case _Base() => _Base()
          case _Arrow(t1, t2) => _Arrow(f(t1), f(t2))
          case _Poly(t) => _Poly(f(t))
        }

      def foldMap[A,B](fa: _Type[A])(f: A => B)(implicit F: Monoid[B]): B =
        fa match {
          case _Base() => F.zero
          case _Arrow(t1, t2) => F.append(f(t1), f(t2))
          case _Poly(t) => f(t)
        }
    }

  val typeAbt: IAbt[_Type] = new Abt

  //Needed reexports
  type Type = typeAbt.Term
  val TVar = typeAbt.Var
  implicit val TermOps = typeAbt.TermOps _
  import typeAbt._

  val Base: Type = _Tm(_Base())

  object Arrow {
    def apply(t1: Type, t2: Type) =
      _Tm(_Arrow(t1, t2))
    def unapply(t: Type): Option[(Type, Type)] = t match {
      case _Tm(_Arrow(t1, t2)) => Some((t1, t2))
      case _ => None
    }
  }

  object Poly {
    def apply(name: Name, t: Type) =
      _Tm(_Poly(_Abs(name, t)))
    def unapply(t: Type): Option[(Name, Type)] = t match {
      case _Tm(_Poly(_Abs(name, t))) => Some((name, t))
      case _ => None
    }
  }
}

object MiniMLTypes {
  import MiniMLTypeSyn._

  def isMono(t: Type): Boolean = t match {
    case Base => true
    case Arrow(t1, t2) => isMono(t1) && isMono(t2)
    case Poly(_, _) => false
  }

  //Following MiniML, we don't bind type variables in the context, we just use
  //them free.
  case class Context(asMap: Map[Name, Type]) {
    require(asMap forall {
      case (_, typ) => isMono(typ)
    })
  }

  def generalize(ctx: Context, t: Type): Type = {
    assert(isMono(t))
    val freeVars = (t.freeVars -- ctx.asMap.values.flatMap(_.freeVars)).toList.sorted
    freeVars.foldRight(t)(Poly(_, _))
  }

  def instantiate(t: Type, ctx: Context): Type = {
    t.subst(ctx.asMap)
  }

}

object MiniMLTermSyn {
  import MiniMLTypeSyn._

  protected sealed trait _TLambda[T]
  private case class _Lam[T](t: T) extends _TLambda[T]
  private case class _App[T](t1: T, t2: T) extends _TLambda[T]
  private case class _Let[T](t1: T, t2: T) extends _TLambda[T]
  private case class _Annot[T](t: T, tp: Type) extends _TLambda[T]

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
    def apply(t: Term, tp: Type): Term =
      _Tm(_Annot(t, tp))
    def unapply(t: Term): Option[(Term, Type)] = t match {
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
