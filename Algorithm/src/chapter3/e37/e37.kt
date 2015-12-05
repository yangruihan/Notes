package e37

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun coeff(a: IntArray, n: Int) {
    if (n < 2) {
        a[1] = 1
        a[2] = 1
    } else {
        coeff(a, n - 1)
        a[n + 1] = 1
        for (i in n downTo 2)
            a[i] += a[i - 1]
        a[1] = 1
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = IntArray(100)
    val n = scanner.nextInt()
    coeff(a, n)
    for (i in 1..(n + 1))
        print("${a[i]} ")
}