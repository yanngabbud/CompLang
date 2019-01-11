/*
 * Hello object defines foo method
 */
object Hello {
  def foo(a: Boolean, x: Int, y: Int, z: Boolean): Boolean = {
    val add: Int = x + y;
    if (0 < x) {
      true
    }
    else {
      if (a) {
        add match {
          case 0 => false
          case 1 => true
          case _ =>
            // if 1
            if (z) { // if 2
              // if 3
              true
            }
            // else 1
            else { // else 2
              // else 3
              error("wrong input")
            }
        }
      } else {
        false
      }
    }
  }
}

