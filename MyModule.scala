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

object Search {
    // key is our target value
    // gt is a function that checks if one value is greater than the other
    // this is a polymorphic function (we can pass in any type)
    def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
        @annotation.tailrec
        def go(low: Int, mid: Int, high: Int): Int = {
            if (low > high) -mid - 1
            else {
                val mid2 = (low + high) / 2
                val a = as(mid2)
                val greater = gt(a, key)
                if (!greater && !gt(key, a)) mid2
                else if (greater) go(low, mid2, mid2-1)
                else go(mid2+1, mid2, high)
            }
        }
        go(0, 0, as.length-1)
    }
}