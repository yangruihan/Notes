package e15

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    var a = IntArray(100)
    var b = IntArray(100)
    var c = IntArray(100)
    for (i in 0..n - 1) {
        a[i] = scanner.nextInt()
        b[i] = a[i]
        c[i] = i
    }
    for (i in 1..n - 1)
        if (b[i - 1] + b[i] > b[i]) {
            b[i] += b[i - 1]
            c[i] = c[i - 1]
        }

    var max = 0
    var j = 0
    for (i in 0..n - 1)
        if (max < b[i]) {
            max = b[i]
            j = i
        }

    print("The max sum is ${b[j]}\n")
    for (i in c[j]..j)
        print("${a[i]}\t")
}