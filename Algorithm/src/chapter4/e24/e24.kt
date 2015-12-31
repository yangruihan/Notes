package e24

import java.util.*

/**
 * Created by Yrh on 2015/12/12.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)

    print("Input number n: ")
    val n = scanner.nextInt()

    var a = Array(3, { Array(n, { IntArray(n) }) })

    for (i in 0..n - 1) {
        for (j in 0..i) {
            a[0][i][j] = scanner.nextInt()
            a[2][i][j] = a[0][i][j]
        }
    }

    for (i in n - 2 downTo 0) {
        for (j in 0..i) {
            when {
                a[0][i + 1][j] > a[0][i + 1][j + 1] -> {
                    a[1][i][j] = 1
                    a[0][i][j] += a[0][i + 1][j]
                }
                else -> {
                    a[1][i][j] = 2
                    a[0][i][j] += a[0][i + 1][j + 1]
                }
            }
        }
    }

    print("The result is ${a[0][0][0]}\n")

    var i = 0
    var j = 0
    print("${a[2][i][j]}")
    while(a[1][i][j] != 0) {
        when(a[1][i++][j]) {
            2 -> j++
        }
        print("->${a[2][i][j]}")
    }
}
