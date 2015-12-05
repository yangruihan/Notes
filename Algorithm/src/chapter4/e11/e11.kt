package e11

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val a = scanner.nextInt()
    val b = scanner.nextInt()
    val c = scanner.nextInt()
    // i 是 a,b,c 中的最大值
    var i = when {
        a > b && a > c -> a
        b > a && b > c -> b
        else -> c
    }
    while (true) {
        if (i % a == 0 && i % b == 0 && i % c == 0) break
        else i++
    }
    print("$a $b $c least common multiple is $i")
}
