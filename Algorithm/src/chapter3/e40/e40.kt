package e40

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun f(n: Int): Int {
    return when (n) {
        1 -> 1
        2 -> 2
        else -> f(n-1) + f(n-2)
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    println("f($n) = ${f(n)}")
}