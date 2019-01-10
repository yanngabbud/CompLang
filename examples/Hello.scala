object /* TEST */Hello { /* TEST */
  /* TEST */
  /* TEST */ /* TEST */
  def foo(a: Boolean, x: Int, y: Int, z: /* TEST */Boolean): Boolean = { // TEST -7
    /* SS */ // TEST -6
    // TEST -5
    val add: Int /* TEST -4 */= x+y;
    /* TEST */if /* TEST */(0/* TEST */<x) {
      true
    } // TEST -3
    // TEST -2
    /* TEST */else { // TEST -1
      /* TEST */
      /* TEST */if (a) { // TEST 0
        // TEST 1
        add match { // TEST 2
          // TEST 3
          case /* TEST */0 /* TEST */=> false // TEST 4
          case 1 => true
          case _ =>
            if (z) {
              true // TEST 5
            } else {
              /* TEST */error("wrong input")/* TEST */
            }
        }
      }
      else { false }
    }
  }
}

