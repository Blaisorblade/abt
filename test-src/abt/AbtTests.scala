package abt

import examples.LambdaCalc._

import org.scalatest.FunSuite

/**
 * @author pgiarrusso
 */
class AbtTests extends FunSuite {
  test("alpha-equiv distinguishes different terms") {
    assert(
      !(Lam("x", App(Var("x"), Var("x"))) alphaEquiv
        Lam("y", Var("y"))))
  }

  test("alpha-equiv works on closed terms") {
    assert(
      Lam("x", Var("x")) alphaEquiv Lam("y", Var("y")))
    assert(
      Lam("x", App(Var("x"), Var("x"))) alphaEquiv
        Lam("y", App(Var("y"), Var("y"))))
  }

  test("alpha-equiv works on open terms") {
    assert(!(Lam("x", Var("x")) alphaEquiv Lam("y", Var("x"))))
    assert(!(Lam("x", Var("x")) alphaEquiv Lam("x", Var("y"))))
    assert(!(Lam("y", Var("x")) alphaEquiv Lam("x", Var("y"))))
  }

  test("alpha-equiv is a no-op when substituting unused variables") {
    val term = Lam("x", App(Var("x"), Var("y")))
    assert((term subst ("z", Var("w"))) == term)
  }

  test("alpha-equiv should not rename variables needlessly") {
    assert((Lam("x", App(Var("x"), Var("y"))) subst ("y", Var("z"))) ==
      Lam("x", App(Var("x"), Var("z"))))
  }

  test("substitution testcases") {
    assert(Lam("x", Var("y")) subst ("y", Var("x")) alphaEquiv Lam("y", Var("x")))
    assert(Lam("x", App(Var("y"), Lam("y", App(Var("y"), App(Var("x"), Var("z")))))) subst ("y", Var("x")) alphaEquiv
        Lam("w", App(Var("x"), Lam("y", App(Var("y"), App(Var("w"), Var("z")))))))
  }
}
