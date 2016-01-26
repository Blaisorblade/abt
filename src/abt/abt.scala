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
import language.implicitConversions
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
  case class Name(s: String, n: Int = 0) {
    override def toString = s"${s}_${n}"
  }

  object Name {
    implicit val nOrdering: Ordering[Name] = Ordering.by { name => (name.s, name.n) }
  }

  implicit def toName(s: String) = Name(s)
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
    @inline def freeVars: Names = _freeVars(t)
    @inline def subst(v: Name, inner: Term): Term = _subst(t, v, inner)
    @inline def alphaEquiv(other: Term): Boolean = _alphaEquiv(t, other)
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

  object _Tm {
    def apply(t: Signature[Term]): Term = _mkTm(t)
    def unapply(t: Term): Option[Signature[Term]] = t match {
      case _Term(__Tm(t1)) => Some(t1)
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

  //Methods for internal usage, outside of the interface.
  //Concrete type used to build terms.
  protected sealed trait __Term[T]
  //This is used by smart constructors of all binders.
  protected case class __Abs[T](n: Name, t: T) extends __Term[T]

  protected case class _Var[A](n: Name) extends __Term[A]
  //This is used by smart constructors of all other operations.
  protected case class __Tm[T](t: Signature[T]) extends __Term[T]

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
    case __Tm(t) => __Tm(Functor[Signature].map(t)(f))
  }

  type Term = TermInt
  case class TermInt(vars: Names, t: __Term[Term])

  def _into(t: __Term[Term]): Term =
    t match {
      case _Var(n) => _mkVar(n)
      case __Abs(n, body) => _mkAbs(n, body)
      case __Tm(t) => _mkTm(t)
    }

  def _out(t: Term): __Term[Term] = t.t

  def _mkVar(n: Name): Term = TermInt(Set(n), _Var(n))
  def _mkAbs(n: Name, body: Term): Term =
    TermInt(_freeVars(body) - n, __Abs(n, body))

  def _mkTm(t: Signature[Term]): Term =
    TermInt(Foldable[Signature].fold(Functor[Signature].map(t)(_freeVars)), __Tm(t))

  def _freeVars(t: Term): Names = t.vars

  var index = 0
  def fresh(): Name = {
    index += 1
    Name("x", index)
  }
  def fresh(baseName: Name, vars: Names): Name =
    if (vars contains baseName) {
      val Name(name, idx) = baseName
      fresh(Name(name, idx + 1), vars)
    } else
      baseName

  def _substQuadratic(outer: Term, v: Name, inner: Term): Term =
    substQuadratic(outer, v, inner, true)

  def children[T](s: Signature[T]): Vector[T] =
    Foldable[Signature].fold(Functor[Signature].map(s)(Vector(_)))

  //This is a basic substitution with quadratic complexity.
  def substQuadratic(outer: Term, v: Name, inner: Term, preRename: Boolean): Term =
    _out(outer) match {
      case _Var(name) if v == name => inner
      case __Tm(body) =>
        _Tm(Functor[Signature].map[Term, Term](body)(x => substQuadratic(x, v, inner, preRename)))
      case __Abs(name, body) if v != name =>
        val (name1, body1) =
          if (preRename) {
            val newName = fresh(name, _freeVars(body) ++ _freeVars(inner))
            val newBody = substQuadratic(body, name, Var(newName), false)
            (newName, newBody)
          } else {
            //We're replacing
            (name, body)
          }
        val body2 = substQuadratic(body1, v, inner, preRename)
        _Abs(name1, body2)
      case _ => //For when guards fail
        outer
    }

  def _subst(outer: Term, v: Name, inner: Term): Term =
    subst(outer, _freeVars(inner), Map(v -> inner))

  /**
   * Parallel substitution.
   * Precondition:
   *   map.values.flatMap(_freeVars).toSet == fvInners
   */
  def subst(outer: Term, fvInners: Set[Name], map: Map[Name, Term]): Term = {
    assert(map.values.flatMap(_freeVars).toSet.subsetOf(fvInners),
      s"!${map.values.flatMap(_freeVars).toSet}.subsetOf($fvInners)")
    //Even stronger assertion
    assert(map.values.flatMap(_freeVars).toSet == fvInners,
      s"${map.values.flatMap(_freeVars).toSet} != $fvInners")
    _out(outer) match {
      case _Var(name) =>
        map get name getOrElse outer
      case __Tm(body) =>
        _Tm(Functor[Signature].map[Term, Term](body)(x => subst(x, fvInners, map)))
      case __Abs(name, body) =>
        val newName = fresh(name, (_freeVars(body) - name) ++ fvInners)
        val varsToRemove = map get name map _freeVars getOrElse Set()
        val newBody = subst(body, fvInners -- varsToRemove + newName, map + (name -> Var(newName)))
        _Abs(newName, newBody)
      case _ => //For when guards fail
        outer
    }
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
      case (__Tm(s1), __Tm(s2)) =>
        val c1 = children(s1)
        val c2 = children(s2)
        c1.length == c2.length && (c1 zip c2).forall {
          case (t1, t2) => alphaEquivLoop(t1, t2, map1, map2)
        }
      case _ => false
    }
  }
}
