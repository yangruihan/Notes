package e35

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    var t = 1
    var w1 = 0
    for (i in 1..n) {
        println("$i box take $t units.")
        w1 += t
        t *= 2
    }

    w1 *= 100
    println("normal weight $w1")
    println("Input reality weight: ")
    val w2 = scanner.nextInt()
    w1 = (w1 - w2) / 10
    while (w1 != 0) {
        var k = 0
        t = 1
        while (w1 - t >= 0) {
            t *= 2
            k++
        }
        println("$k box is bad")
        w1 -= t / 2
    }
}
