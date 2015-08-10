// Translate https://gist.github.com/neel-krishnaswami/834b892327271e348f79
package abt
import language.higherKinds

import scalaz.Functor

// Semi-standard typeclasses.
// TODO: Refactor to use Scalaz/Cats {{{
trait Foldable[F[T]] {
  def fold[T](v: F[T])(implicit m: Monoid[T]): T
}

trait Monoid[T] {
  def unit: T
  def join(a: T, b: T): T
}

object Monoid {
  implicit def setMonoid[T] =
    new Monoid[Set[T]] {
      def unit = Set.empty
      def join(a: Set[T], b: Set[T]): Set[T] = a ++ b
    }
}

trait ABTs {
  type Name = String
  type Vars = Set[Name]
  /* XXX: Don't use I for interfaces. What's the right convention? */
  trait IAbt[Signature[T]] {
    //Concrete type used to build terms.
    sealed trait BindingTerm[T]
    case class Var[A](n: Name) extends BindingTerm[A]
    case class Abs[T](n: Name, t: T) extends BindingTerm[T]
    case class Tm[T](t: Signature[T]) extends BindingTerm[T]
    def map[A, B](f: A => B): BindingTerm[A] => BindingTerm[B]

    //Abstract type of terms
    type Term

    //Term is isomorphic to BindingTerm[Term]
    def into(t: BindingTerm[Term]): Term
    def out(t: Term): BindingTerm[Term]

    def freeVars(t: Term): Vars

    def makeVar(n: Name): Term
    def makeAbs(n: Name, body: Term): Term
    def makeTm(t: Signature[Term]): Term
    def subst(outer: Term, v: Name, inner: Term): Term
  }

  class Abt[Signature[_]](implicit FoldableSignature: Foldable[Signature], FuncSign: Functor[Signature])
      extends IAbt[Signature] {
    def map[A, B](f: A => B): BindingTerm[A] => BindingTerm[B] = {
      case Var(n) => Var(n)
      case Abs(n, body) => Abs(n, f(body))
      case Tm(t) => Tm(FuncSign.map(t)(f))
    }

    case class Term(vars: Vars, t: BindingTerm[Term])

    def into(t: BindingTerm[Term]): Term =
      t match {
        case Var(n) => makeVar(n)
        case Abs(n, body) => makeAbs(n, body)
        case Tm(t) => makeTm(t)
      }

    def out(t: Term): BindingTerm[Term] = t.t

    def makeVar(n: Name): Term = Term(Set(n), Var(n))
    def makeAbs(n: Name, body: Term): Term =
      Term(freeVars(body) - n, Abs(n, body))

    def makeTm(t: Signature[Term]): Term =
      Term(FoldableSignature.fold(FuncSign.map(t)(freeVars)), Tm(t))

    def freeVars(t: Term): Vars = t.vars

    var index = 0
    def fresh(): Name = {
      index += 1
      "x" + index
    }
    def fresh(baseName: Name, vars: Vars): Name =
      if (vars contains baseName)
        fresh(baseName + "'", vars)
      else
        baseName

    def subst(outer: Term, v: Name, inner: Term): Term =
      subst(outer, v, inner, true)

    //This is a basic substitution with quadratic complexity.
    def subst(outer: Term, v: Name, inner: Term, preRename: Boolean): Term =
      out(outer) match {
        case Var(name) if v == name => inner
        case Tm(body) =>
          makeTm(FuncSign.map[Term, Term](body)(x => subst(x, v, inner, preRename)))
        case Abs(name, body) if v != name =>
          val (name1, body1) =
            if (preRename) {
              val newName = fresh(name, freeVars(body) ++ freeVars(outer))
              val newBody = subst(body, name, makeVar(newName), false)
              (newName, newBody)
            } else {
              //We're replacing
              (name, body)
            }
          val body2 = subst(body1, v, inner, preRename)
          makeAbs(name1, body2)
        case _ => //For when guards fail
          outer
      }
  }

  trait SimpleType
  case object Base extends SimpleType
  case class Arrow(t1: SimpleType, t2: SimpleType) extends SimpleType

  sealed trait TLambda[T]
  case class Abs[T](t: T) extends TLambda[T]
  case class App[T](t1: T, t2: T) extends TLambda[T]
  case class Let[T](t1: T, t2: T) extends TLambda[T]
  case class Annot[T](tp: SimpleType, t: T) extends TLambda[T]

  implicit val lambdaSig: Functor[TLambda] with Foldable[TLambda] =
    new Functor[TLambda] with Foldable[TLambda] {
      def map[A, B](fa: TLambda[A])(f: A => B): TLambda[B] = fa match {
        case Abs(t) => Abs(f(t))
        case Annot(tp, t) => Annot(tp, f(t))
        case App(t1, t2) => App(f(t1), f(t2))
        case Let(t1, t2) => Let(f(t1), f(t2))
      }

      def fold[T](v: TLambda[T])(implicit m: Monoid[T]): T =
        v match {
          case Abs(t) => t
          case Annot(_, t) => t
          case App(t1, t2) => m.join(t1, t2)
          case Let(t1, t2) => m.join(t1, t2)
        }
    }

  val lambdaAbt: IAbt[TLambda] = new Abt
}
