/*
* Hello object defines foo method
*/
object Hello {
// foo method
def foo(a: Boolean, x: Int, y: Int, z: Boolean): Boolean = {
val add: Int = x + y;
if (0 < x) {
true
} else {
if (a) {
add match { // a match
case 0 => false
case 1 => true // case one
case _ =>
// if
if (z) {
true
}
// else
else {
error("wrong input")
}
}
} else {
false
}
}
}
}