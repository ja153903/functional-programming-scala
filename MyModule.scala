object MyModule {
    def abs(n: Int): Int = {
        if (n < 0) { -n }
        else { n }
    }

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is is %d"
        msg.format(x, abs(x))
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit = {
        //println(formatAbs(-42))
        println(formatResult("absolute value", 42, abs))
    }
}

object Factorial {
    def factorial(x: Int): Int = {
        def go(x: Int, acc: Int): Int = {
            if (x <= 1) { acc }
            else { go(x-1, acc*x) }
        }
        go(x, 1)
    }

    def main(args: Array[String]): Unit = {
        println(factorial(4))
    }
}

object Fibonacci {
    def fibonacci(n: Int): Int = {
        def loop(n: Int, a: Int, b: Int): Int = {
            if (n == 2) { b }
            else { loop(n-1, b, a+b) }
        }
        loop(n, 1, 1)
    }

    def main(args: Array[String]): Unit = {
        println(fibonacci(7))
    }
}