package e41

import java.util.*

/**
 * Created by Yrh on 2015/12/3.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var n = IntArray(100)
    var m = IntArray(100)
    val t = scanner.nextInt()
    n[0] = 1
    m[0] = 0
    for (i in 1..t) {
        n[i] = m[i-1]
        m[i] = 3*n[i-1]+2*m[i-1]
    }

    println(n[t])
    println(m[t])
}
