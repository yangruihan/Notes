package e4

import java.util.*

/**
 * Created by Yrh on 2015/12/4.
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = IntArray(100)
    val n = scanner.nextInt()
    when {
        n == 1 -> print("1")
        n == 2 -> print("1\n1\t1")
        n >= 3 -> {
            print("1\n1\t1\n")
            a[0] = 1
            a[1] = 1
            for (i in 3..n) {
                a[i - 1] = 1
                a[0] = 1
                for (j in i - 2 downTo 1) {
                    a[j] = a[j] + a[j-1]
                }
                for (j in 0..i-1)
                    print("${a[j]}\t")
                print("\n")
            }
        }
        else -> print("Input Error")
    }
}