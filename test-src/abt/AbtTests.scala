package abt

import Lambda._
import Lambda.lambdaAbt._

import org.scalatest.FunSuite

/**
 * @author pgiarrusso
 */
class AbtTests extends FunSuite {
  test("alpha-equiv works") {
    assert(alphaEquiv(
      lambda("x", var_("x")),
      lambda("y", var_("y"))))
    assert(!alphaEquiv(
      lambda("x", app(var_("x"), var_("x"))),
      lambda("y", var_("y"))))
    assert(alphaEquiv(
      lambda("x", app(var_("x"), var_("x"))),
      lambda("y", app(var_("y"), var_("y")))))
    assert(!alphaEquiv(lambda("x", var_("x")), lambda("y", var_("x"))))
    assert(!alphaEquiv(lambda("x", var_("x")), lambda("x", var_("y"))))
    assert(!alphaEquiv(lambda("y", var_("x")), lambda("x", var_("y"))))
  }


  test("substitution testcases") {
    val x = "x"
    assert(alphaEquiv(subst(lambda(x, var_("y")), "y", var_("x")), lambda("y", var_("x"))))
  }
}
