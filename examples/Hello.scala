object Hello {
  abstract class test
  case class testtest(x: Int) extends test
  def foo(a: Boolean, x: Int, y: Int, z: Boolean): Boolean = {
    val add: Int = x + y;
    if (0 < x) {
      true
    } else {
      if (a) {
        add match {
          case 0 =>
            val test: Int = x + y;
            false
          case 1 => true
          case _ =>
            if (z) {
              true
            } else {
              error("wrong input")
            }
        }
      }
      else { false }
    }
  }
  val toto: Boolean = foo(true, 0, 1, false);
  foo(true, 0, 1, false)
}

