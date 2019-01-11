/*
 * Hello object defines foo method
 */
object Hello {
  def /* weird comment */foo(/* weird comment */a: Boolean, x: Int, y: Int, z: Boolean)/* weird comment */: Boolean = {
    val /* weird comment */add: /* weird comment */ Int = /* weird comment */x + y;
    if (0/* weird comment */ < x) {
      true
    }
    else {
      if (/* weird comment */a) {
        add match {
          case /* weird comment */0 => false
          case 1 => true
          case _ =>
            if/* weird comment */ (z) {
              true
            }
            else {
              error/* weird comment */("wrong input")
            }
        }
      } else/* weird comment */ {
        false
      }
    }
  }
}

