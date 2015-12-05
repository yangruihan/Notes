package e8

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    print("Input x1, x2 (f(x1) * f(x2) < 0): ")
    var x1 = scanner.nextFloat()
    var x2 = scanner.nextFloat()
    var x: Float
    val f = { x: Float -> Math.pow(x * 1.0, 3.0) / 2 + 2 * Math.pow(x * 1.0, 2.0) - 8 }
    loop@ do {
        x = (x1 + x2) / 2
        when {
            f(x) == 0.0 -> break@loop
            f(x1) * f(x) > 0 -> x1 = x
            f(x2) * f(x) > 0 -> x2 = x
        }
    } while (Math.abs(f(x)) >= 1e-4)
    print("The result is $x")
}