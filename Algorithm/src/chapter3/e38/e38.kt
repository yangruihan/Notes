package e38

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun ff(a: Int, b: Int): Int {
    var t = 1
    var i = 2
    var a1 = a
    var b1 = b
    while (i <= a1 && i <= b1) {
        t *= i
        a1 /= i
        b1 /= i
        i++
    }
    return t
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = IntArray(100)
    print("Input the number of data: ")
    val n = scanner.nextInt()
    print("Input the distant of moving: ")
    val k = scanner.nextInt()
    for (i in 0..n - 1)
        a[i] = scanner.nextInt()

    // 求n和k的最大公约数
    val m = ff(n, k)

    for (j in 0..m - 1) {
        var b0 = a[j]
        var tt = j
        var b1: Int
        for (i in 0..n / m - 1) {
            tt = (tt + k) % n
            b1 = a[tt]
            a[tt] = b0
            b0 = b1
        }
    }
    for (i in 0..n - 1) {
        print("${a[i]} ")
    }
}