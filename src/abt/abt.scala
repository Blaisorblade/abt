// Translate https://gist.github.com/neel-krishnaswami/834b892327271e348f79
/**
  * Support for *Abstract Binding Trees*, or ABTs.
  *
  * To implement a language using Abstract Binding Trees:
  * 1. First, you provide an underlying algebric data type
  * Signature representing the abstract syntax of your language, and provide an
  * implementation of Functor[Signature] and Foldable[Signature]. This abstract
  * syntax definition contains no binding information; for instance,
  * a lambda abstraction is defined as a node _Lam(t: Term).
  * 2. Then, you provide smart constructors/extractors exposing the intended
  * interface to the client. These smart constructors will assemble together AST
  * nodes with binding nodes. For instance, you can provide constructors/extractors
  * for Lam, that assemble together a _Lam node with an abstractor.
  *
  * See Lambda for an example.
  */
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

/**
  * Interface for Abstract Binding Trees. To specialize them to a specific
  * datatype, you need to define extra constructors, and smart constructors
  * using Abs in the right places (the ones where, in HOAS, you'd use an open
  * term Term => Term).
  *
  * Members are divided in three groups:
  * - Some members can be re-exported by the implementation of the client
  *   language; their name starts without underscores.
  * - Some members are to be used by the implementation of the client
  *   language. Since that is no subclass, they cannot be made protected;
  *   instead their names start with underscores.
  * - Some members are just to be implemented by concrete ABT implementations;
  *   they are marked as protected and their name start with one or more
  *   underscores.
  *
  * @param Signature the signature of the underlying AST.
  */
/* XXX: Don't use I for interfaces. What's the right convention? */
trait IAbt[Signature[_]] {
  outer =>

  /*
   * Members to reexport to clients.
   */
  //Abstract type of terms
  type Term

  //Smart constructors/extractors.

  //This one will typically be part of your language.
  object Var {
    def apply(n: Name): Term = _mkVar(n)
    def unapply(t: Term): Option[Name] = t match {
      case _Term(_Var(n)) => Some(n)
      case _ => None
    }
  }

  implicit class TermOps(t: Term) {
    def freeVars: Names = _freeVars(t)
    def subst(v: Name, inner: Term): Term = _subst(t, v, inner)
    def alphaEquiv(other: Term): Boolean = _alphaEquiv(t, other)
  }

  /*
   * Members for use by language implementations.
   */
  /**
    * Term is isomorphic to __Term[Term], and this is the witness.
    * This isomorphism is not exposed to language clients.
    */
  object _Term {
    def apply(t: __Term[Term]): Term = _into(t)
    def unapply(t: Term): Some[__Term[Term]] = Some(_out(t))
  }

  object _TermSig {
    def apply(t: Signature[Term]): Term = _Term(_Tm(t))
    def unapply(t: Term): Option[Signature[Term]] = t match {
      case _Term(_Tm(t1)) => Some(t1)
      case _ => None
    }
  }

  object _Abs {
    def apply(n: Name, body: Term): Term = _mkAbs(n, body)
    def unapply(t: Term): Option[(Name, Term)] = t match {
      case _Term(__Abs(n, body)) => Some((n, body))
      case _ => None
    }
  }

  protected def _freeVars(t: Term): Names
  protected def _subst(outer: Term, v: Name, inner: Term): Term
  protected def _alphaEquiv(t1: Term, t2: Term): Boolean

  //Methods for internal usage, outside of the interface interface
  //Concrete type used to build terms.
  protected sealed trait __Term[T]
  //This is used by smart constructors of all binders.
  protected case class __Abs[T](n: Name, t: T) extends __Term[T]

  protected case class _Var[A](n: Name) extends __Term[A]
  protected case class _Tm[T](t: Signature[T]) extends __Term[T]

  protected def map[A, B](bt: __Term[A])(f: A => B): __Term[B]

  protected object __Term {
    implicit val isFunctor =
      new Functor[__Term] {
        def map[A, B](bt: __Term[A])(f: A => B): __Term[B] = outer.map(bt)(f)
      }
  }

  protected def _into(t: __Term[Term]): Term
  protected def _out(t: Term): __Term[Term]

  protected def _mkVar(n: Name): Term
  protected def _mkAbs(n: Name, body: Term): Term
  protected def _mkTm(t: Signature[Term]): Term
}

class Abt[Signature[_]: Functor: Foldable] extends IAbt[Signature] {
  def map[A, B](bt: __Term[A])(f: A => B): __Term[B] = bt match {
    case _Var(n) => _Var(n)
    case __Abs(n, body) => __Abs(n, f(body))
    case _Tm(t) => _Tm(Functor[Signature].map(t)(f))
  }

  type Term = TermInt
  case class TermInt(vars: Names, t: __Term[Term])

  def _into(t: __Term[Term]): Term =
    t match {
      case _Var(n) => _mkVar(n)
      case __Abs(n, body) => _mkAbs(n, body)
      case _Tm(t) => _mkTm(t)
    }

  def _out(t: Term): __Term[Term] = t.t

  def _mkVar(n: Name): Term = TermInt(Set(n), _Var(n))
  def _mkAbs(n: Name, body: Term): Term =
    TermInt(_freeVars(body) - n, __Abs(n, body))

  def _mkTm(t: Signature[Term]): Term =
    TermInt(Foldable[Signature].fold(Functor[Signature].map(t)(_freeVars)), _Tm(t))

  def _freeVars(t: Term): Names = t.vars

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

  def _subst(outer: Term, v: Name, inner: Term): Term =
    subst(outer, v, inner, true)

  def children[T](s: Signature[T]): Vector[T] =
    Foldable[Signature].fold(Functor[Signature].map(s)(Vector(_)))

  //This is a basic substitution with quadratic complexity.
  def subst(outer: Term, v: Name, inner: Term, preRename: Boolean): Term =
    _out(outer) match {
      case _Var(name) if v == name => inner
      case _Tm(body) =>
        _mkTm(Functor[Signature].map[Term, Term](body)(x => subst(x, v, inner, preRename)))
      case __Abs(name, body) if v != name =>
        val (name1, body1) =
          if (preRename) {
            val newName = fresh(name, _freeVars(body) ++ _freeVars(inner))
            val newBody = subst(body, name, Var(newName), false)
            (newName, newBody)
          } else {
            //We're replacing
            (name, body)
          }
        val body2 = subst(body1, v, inner, preRename)
        _Abs(name1, body2)
      case _ => //For when guards fail
        outer
    }

  def _alphaEquiv(t1: Term, t2: Term): Boolean = {
    alphaEquivLoop(t1, t2, Map(), Map())
  }

  /**
    * A linear-time implementation of alpha-equivalence checking.
    */
  def alphaEquivLoop(t1: Term, t2: Term, map1: Map[Name, Name], map2: Map[Name, Name]): Boolean = {
    (_out(t1), _out(t2)) match {
      case (_Var(n1), _Var(n2)) =>
        (map1 get n1, map2 get n2) match {
          case (Some(renamedN1), Some(renamedN2)) => renamedN1 == renamedN2
          case (None, None) => n1 == n2
          case _ => false
        }
      case (__Abs(n1, b1), __Abs(n2, b2)) =>
        val freshName = fresh()
        alphaEquivLoop(b1, b2, map1 + (n1 -> freshName), map2 + (n2 -> freshName))
      case (_Tm(s1), _Tm(s2)) =>
        val c1 = children(s1)
        val c2 = children(s2)
        c1.length == c2.length && (c1 zip c2).forall {
          case (t1, t2) => alphaEquivLoop(t1, t2, map1, map2)
        }
      case _ => false
    }
  }
}

/**
  * A Curry-style typed lambda calculus, implemented using ABTs.
  */
object Lambda {
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
  implicit val TermOps = lambdaAbt.TermOps _

  //
  import lambdaAbt._
  //Smart constructors/extractors
  object Lam {
    def apply(name: Name, body: Term): Term =
      _TermSig(_Lam(_Abs(name, body)))
    def unapply(t: Term): Option[(Name, Term)] = t match {
      case _TermSig(_Lam(_Abs(name, body))) => Some((name, body))
      case _ => None
    }
  }

  object App {
    def apply(f: Term, arg: Term): Term =
      _TermSig(_App(f, arg))
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case _TermSig(_App(f, arg)) => Some((f, arg))
      case _ => None
    }
  }

  object Annot {
    def apply(t: Term, tp: SimpleType): Term =
      _TermSig(_Annot(t, tp))
    def unapply(t: Term): Option[(Term, SimpleType)] = t match {
      case _TermSig(_Annot(t, tp)) => Some((t, tp))
      case _ => None
    }
  }

  object Let {
    def apply(name: Name, t1: Term, t2: Term): Term =
      _TermSig(_Let(t1, _Abs(name, t2)))
    def unapply(t: Term): Option[(Name, Term, Term)] = t match {
      case _TermSig(_Let(t1, _Abs(name, t2))) => Some((name, t1, t2))
      case _ => None
    }
  }
}

object Bidir {
  import Lambda._

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
