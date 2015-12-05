package e12

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    for (i in 1..n)
        if (i * i <= n)
            print("${i * i} is free\n")
}