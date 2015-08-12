package abt

import Lambda._

import org.scalatest.FunSuite

/**
 * @author pgiarrusso
 */
class AbtTests extends FunSuite {
  test("alpha-equiv works") {
    assert(
      Lam("x", Var("x")) alphaEquiv Lam("y", Var("y")))
    assert(
      !(Lam("x", App(Var("x"), Var("x"))) alphaEquiv
        Lam("y", Var("y"))))
    assert(
      Lam("x", App(Var("x"), Var("x"))) alphaEquiv
        Lam("y", App(Var("y"), Var("y"))))
    assert(!(Lam("x", Var("x")) alphaEquiv Lam("y", Var("x"))))
    assert(!(Lam("x", Var("x")) alphaEquiv Lam("x", Var("y"))))
    assert(!(Lam("y", Var("x")) alphaEquiv Lam("x", Var("y"))))
  }


  test("substitution testcases") {
    assert(Lam("x", Var("y")) subst ("y", Var("x")) alphaEquiv Lam("y", Var("x")))
  }
}
