package abt

import Lambda._
import Bidir._

import org.scalatest.FunSuite

/**
 * @author pgiarrusso
 */
class BidirTests extends FunSuite {
  test("Simple tests for bidirectional type checking") ({
    check(Map("x" -> Base), Var("x"), Base)
    val tId = Arrow(Base, Base)
    check(Map.empty, Lam("x", Var("x")), tId)
    check(Map.empty, Lam("x", Var("x")), Arrow(tId, tId))
    check(Map.empty, Lam("x", Lam("y", App(Var("x"), Var("y")))), Arrow(tId, tId))
    intercept[IllegalArgumentException] {
      check(Map.empty, Lam("x", Var("x")), Arrow(tId, Base))
    }
  })
}
