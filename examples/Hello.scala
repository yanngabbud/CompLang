object Hello {
  def foo(a: Boolean, x: Int, y: Int, z: Boolean): Boolean = { // TEST -7
    /* SS */ // TEST -6
    // TEST -5
    val add: Int /* TEST -4 */= x+y;
    if (0<x) {
      true
    } // TEST -3
    // TEST -2
    else { // TEST -1
      if (a) { // TEST 0
        // TEST 1
        add match { // TEST 2
          // TEST 3
          case 0 => false // TEST 4
          case 1 => true
          case _ =>
            if (z) {
              true // TEST 5
            } else {
              error("wrong input")
            }
        }
      }
      else { false }
    }
  }
}

