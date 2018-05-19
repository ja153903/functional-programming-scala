// case classes are compared by structure and not by reference
case class Point(x: Double = 0.0, y: Double = 0.0) {
    def shift(deltax: Double = 0.0, deltay: Double = 0.0) =
        copy(x + deltax, y + deltay)
}

abstract class Shape() {
    def draw(offset: Point = Point(0.0, 0.0))(f: String => Unit): Unit = {
        f(s"draw(offset=$offset, ${this.toString}")
    }
}

case class Circle(center: Point, radius: Double) extends Shape

case class Rectangle(lowerLeft: Point, height: Double, width: Double) extends Shape


object PartialFunc {

    def tryPF(s: String, f: PartialFunction[Any, String]): String = {
        try { f(s).toString } catch { case _: MatchError => "Error"}
    }

    def main(args: Array[String]): Unit = {
        // this partial function only matches on strings
        val pf1: PartialFunction[Any, String] = { case s: String => "Yes" }

        // this partial function matches on doubles
        val pf2: PartialFunction[Any, String] = { case d: Double => "No"}

        val pf = pf1 orElse pf2

        println(tryPF("hello", pf))

        val p1 = new Point(x=3.3, y=4.4)
        val p2 = p1.copy(y=2.2)
        println(p2.y)
    }

}