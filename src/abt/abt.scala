// Translate https://gist.github.com/neel-krishnaswami/834b892327271e348f79
package abt
import language.higherKinds

import scalaz.Functor
import scalaz.Foldable
import scalaz.Monoid

object ABTs {
  //Not part of Scalaz, unless we switch to Scalaz's Set.
  implicit def setMonoid[T] =
    new Monoid[Set[T]] {
      def zero = Set.empty
      def append(a: Set[T], b: => Set[T]): Set[T] = a ++ b
    }
  implicit def seqMonoid[T] =
    new Monoid[Seq[T]] {
      def zero = Seq.empty
      def append(a: Seq[T], b: => Seq[T]): Seq[T] = a ++ b
    }
  implicit def vectorMonoid[T] =
    new Monoid[Vector[T]] {
      def zero = Vector.empty
      def append(a: Vector[T], b: => Vector[T]): Vector[T] = a ++ b
    }
  type Name = String
  type Names = Set[Name]
}

import ABTs._

/* XXX: Don't use I for interfaces. What's the right convention? */
trait IAbt[Signature[T]] {
  outer =>

  //Concrete type used to build terms.
  sealed trait _Term[T]
  case class Var[A](n: Name) extends _Term[A]
  case class Abs[T](n: Name, t: T) extends _Term[T]
  case class Tm[T](t: Signature[T]) extends _Term[T]
  def map[A, B](bt: _Term[A])(f: A => B): _Term[B]

  object _Term {
    implicit val isFunctor =
      new Functor[_Term] {
        def map[A, B](bt: _Term[A])(f: A => B): _Term[B] = outer.map(bt)(f)
      }
  }

  //Abstract type of terms
  type Term
  object Term {
    def apply(t: _Term[Term]): Term = into(t)
    def unapply(t: Term): Some[_Term[Term]] = Some(out(t))
  }
  object TermSig {
    def apply(t: Signature[Term]): Term = Term(Tm(t))
    def unapply(t: Term): Option[Signature[Term]] = t match {
      case Term(Tm(t1)) => Some(t1)
      case _ => None
    }
  }

  //Term is isomorphic to _Term[Term]
  def into(t: _Term[Term]): Term
  def out(t: Term): _Term[Term]

  def freeVars(t: Term): Names

  def makeVar(n: Name): Term
  def makeAbs(n: Name, body: Term): Term
  def makeTm(t: Signature[Term]): Term
  def subst(outer: Term, v: Name, inner: Term): Term
  def alphaEquiv(t1: Term, t2: Term): Boolean
}

class Abt[Signature[_]: Functor: Foldable] extends IAbt[Signature] {
  def map[A, B](bt: _Term[A])(f: A => B): _Term[B] = bt match {
    case Var(n) => Var(n)
    case Abs(n, body) => Abs(n, f(body))
    case Tm(t) => Tm(Functor[Signature].map(t)(f))
  }

  type Term = TermInt
  case class TermInt(vars: Names, t: _Term[Term])

  def into(t: _Term[Term]): Term =
    t match {
      case Var(n) => makeVar(n)
      case Abs(n, body) => makeAbs(n, body)
      case Tm(t) => makeTm(t)
    }

  def out(t: Term): _Term[Term] = t.t

  def makeVar(n: Name): Term = TermInt(Set(n), Var(n))
  def makeAbs(n: Name, body: Term): Term =
    TermInt(freeVars(body) - n, Abs(n, body))

  def makeTm(t: Signature[Term]): Term =
    TermInt(Foldable[Signature].fold(Functor[Signature].map(t)(freeVars)), Tm(t))

  def freeVars(t: Term): Names = t.vars

  var index = 0
  def fresh(): Name = {
    index += 1
    "x" + index
  }
  def fresh(baseName: Name, vars: Names): Name =
    if (vars contains baseName)
      fresh(baseName + "'", vars)
    else
      baseName

  def subst(outer: Term, v: Name, inner: Term): Term =
    subst(outer, v, inner, true)

  def children[T](s: Signature[T]): Vector[T] =
    Foldable[Signature].fold(Functor[Signature].map(s)(Vector(_)))

  //This is a basic substitution with quadratic complexity.
  def subst(outer: Term, v: Name, inner: Term, preRename: Boolean): Term =
    out(outer) match {
      case Var(name) if v == name => inner
      case Tm(body) =>
        makeTm(Functor[Signature].map[Term, Term](body)(x => subst(x, v, inner, preRename)))
      case Abs(name, body) if v != name =>
        val (name1, body1) =
          if (preRename) {
            val newName = fresh(name, freeVars(body) ++ freeVars(inner))
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

  //XXX this is also quadratic
  def alphaEquiv(t1: Term, t2: Term): Boolean = {
    (out(t1), out(t2)) match {
      case (Var(n1), Var(n2)) => n1 == n2
      case (Abs(n1, b1), Abs(n2, b2)) =>
        val freshV = makeVar(fresh())
        alphaEquiv(subst(b1, n1, freshV), subst(b2, n2, freshV))
      case (Tm(s1), Tm(s2)) =>
        val c1 = children(s1)
        val c2 = children(s2)
        c1.length == c2.length && ((c1 zip c2) forall ((alphaEquiv _).tupled))
      case _ => false
    }
  }
}

object Lambda {
  trait SimpleType
  case object Base extends SimpleType
  case class Arrow(t1: SimpleType, t2: SimpleType) extends SimpleType

  sealed trait TLambda[T]
  case class Lam[T](t: T) extends TLambda[T]
  case class App[T](t1: T, t2: T) extends TLambda[T]
  case class Let[T](t1: T, t2: T) extends TLambda[T]
  case class Annot[T](tp: SimpleType, t: T) extends TLambda[T]

  implicit val lambdaSig: Functor[TLambda] with Foldable[TLambda] =
    new Functor[TLambda] with Foldable[TLambda] with Foldable.FromFoldMap[TLambda] {
      def map[A, B](fa: TLambda[A])(f: A => B): TLambda[B] = fa match {
        case Lam(t) => Lam(f(t))
        case Annot(tp, t) => Annot(tp, f(t))
        case App(t1, t2) => App(f(t1), f(t2))
        case Let(t1, t2) => Let(f(t1), f(t2))
      }

      def foldMap[A,B](fa: TLambda[A])(f: A => B)(implicit F: Monoid[B]): B =
        fa match {
          case Lam(t) => f(t)
          case Annot(_, t) => f(t)
          case App(t1, t2) => F.append(f(t1), f(t2))
          case Let(t1, t2) => F.append(f(t1), f(t2))
        }
    }

  val lambdaAbt: IAbt[TLambda] = new Abt

  import lambdaAbt._

  def var_(name: Name): Term = Term(Var(name))
  def lambda(name: Name, body: Term): Term =
    TermSig(Lam(Term(Abs(name, body))))
  def app(f: Term, arg: Term): Term =
    TermSig(App(f, arg))
  def annot(t: Term, tp: SimpleType): Term =
    TermSig(Annot(tp, t))
  def let(name: Name, t1: Term, t2: Term): Term =
    TermSig(Let(t1, Term(Abs(name, t2))))
}

object Bidir {
  import Lambda._
  import Lambda.lambdaAbt._

  type Ctx = Map[Name, SimpleType]

  def isSynth: _Term[Term] => Boolean = {
    case Tm(Lam(_)) => false
    case Tm(Let(_, _)) => false
    case _ => true
  }
  def isCheck(bt: _Term[Term]): Boolean = !isSynth(bt)

  def fail(msg: String) =
    throw new IllegalArgumentException(msg)

  def synth(ctx: Ctx, e: Term): SimpleType = {
    e match {
      case Term(Var(x)) =>
        ctx get x getOrElse fail("unbound variable")
      case TermSig(Annot(tp, e)) =>
        check(ctx, e, tp)
        tp
      case TermSig(App(f, e)) =>
        synth(ctx, f) match {
          case Arrow(s, t) =>
            check(ctx, e, s)
            t
          case _ => fail("Applying a non-function!")
        }
      case Term(body) if isCheck(body) =>
        fail("Cannot synthesize type for checking term")
      case _ =>
        fail("Unexpected term")
    }
  }

  def check(ctx: Ctx, e: Term, tp: SimpleType): Unit = {
    (e, tp) match {
      //Lambda
      case (TermSig(Lam(Term(Abs(x, e1)))), Arrow(tp1, tp2)) =>
        check(ctx updated (x, tp1), ??? /*e1*/ , tp2)
      case (TermSig(Lam(t)), _) =>
        fail("Expected arrow type")
      //Let
      case (TermSig(Let(e1, Abs(x, e2))), _) =>
        val tp1 = synth(ctx, e1)
        check(ctx updated (x, tp1), ??? /*e2*/, tp)
      case (Term(body), _) if isSynth(body) =>
        if (tp == synth(ctx, e))
          ()
        else fail("Type mismatch")
      case _ =>
        fail("Unexpected term")
    }
  }
}
