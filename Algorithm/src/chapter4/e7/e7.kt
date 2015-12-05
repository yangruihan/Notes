package e7

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

fun f(a: Float, b: Float, c: Float, d: Float): Float {
    var x1 = 1.0f
    var x0: Float
    do {
        x0 = x1
        x1 = x0 - (((a * x0 + b) * x0 + c) * x0 + d) / ((3 * a * x0 + 2 * b) * x0 + c)
    } while (Math.abs(x1 - x0) >= 1e-4)
    return x1
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    print("Input a, b, c, d: ")
    val a = scanner.nextFloat()
    val b = scanner.nextFloat()
    val c = scanner.nextFloat()
    val d = scanner.nextFloat()
    print("The result is ${f(a, b, c, d)}\n")
}