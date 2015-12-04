package e2

import java.util.*

/**
 * Created by Yrh on 2015/12/4.
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = scanner.nextInt()
    var b = scanner.nextInt()
    while (a % b != 0) {
        val c = a % b
        a = b
        b = c
    }
    print(b)
}