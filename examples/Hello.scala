/*
 * Hello object defines foo method
 */
object Hello { // a match
  // foo method
  def foo(a: Boolean, x: Int, y: Int, z: Boolean): Boolean = { // a match
    val add: /* weird comment */ Int = x+y;
    if (0<x) { true }
    else {
      if (a) { // a match
        add match { // a match
          case 0 => false
          case 1 => true // case one
          case _ => // a match
            if (z) { // an if
              true
            } else { // an else
              error("wrong input")
            }
        }
      }
      else { false }
    }
  }
}

