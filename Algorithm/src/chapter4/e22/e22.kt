package e22

import java.util.*

/**
 * Created by Yrh on 2015/12/12.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)

    val money = intArrayOf(100, 50, 20, 10, 5, 2, 1)
    var count = IntArray(7)

    print("Input the number of people: ")
    val n = scanner.nextInt()

    for (i in 1..n) {
        var salary = scanner.nextInt()
        for (j in 0..6) {
            if (salary / money[j] > 0) {
                count[j] += salary / money[j]
                salary %= money[j]
            }
        }
    }

    print("The result: \n")

    for (i in 0..6) {
        print("${money[i]}: ${count[i]}\n")
    }
}
