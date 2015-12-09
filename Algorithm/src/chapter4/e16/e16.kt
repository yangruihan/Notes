package e16

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

fun carry(data: IntArray, position: Int) {
    if (data[position] < 10) return
    else {
        data[position + 1] += data[position] / 10
        data[position] %= 10
        carry(data, position + 1)
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    print("Input a number: ")
    val s1 = scanner.next() // 乘数
    print("Input another number: ")

    var a = IntArray(100)

    val s2 = scanner.next() // 被乘数
    for ((i, c2) in s2.reversed().withIndex()) {
        for ((j, c1) in s1.reversed().withIndex()) {
            a[i + j] += (c1.toInt() - 48) * (c2.toInt() - 48)
            carry(a, i + j)
        }
    }

    for ((i, v) in a.reversed().withIndex())
        if (v != 0) {
            for (j in  a.size - i - 1 downTo 0) {
                print(a[j])
            }
            break
        }
}
