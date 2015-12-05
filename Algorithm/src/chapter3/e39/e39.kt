package e39

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    println("Please think of a number between 1 and 100.")
    println("Your number divided by 3 has a remainder of: ")
    val a = scanner.nextInt()
    println("Your number divided by 5 has a remainder of: ")
    val b = scanner.nextInt()
    println("Your number divided by 7 has a remainder of: ")
    val c = scanner.nextInt()
    var d = 70 * a + 21 * b + 15 * c
    while (d > 105)
        d -= 105
    println("Your number was $d")
}