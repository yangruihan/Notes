package e36

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    println("Please enter a number: ")
    val n = scanner.nextLong()
    val k = (if (n % 3 == 0L) 1 else 0) + (if (n % 5 == 0L) 2 else 0) + (if (n % 7 == 0L) 4 else 0)
    when (k) {
        7 -> print("All")
        6 -> print("5 and 7")
        5 -> print("3 and 7")
        4 -> print("7")
        3 -> print("3 and 5")
        2 -> print("5")
        1 -> print("3")
        0 -> print("none")
    }
}