package e17

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = IntArray(100)
    print("Input n: ")
    val n = scanner.nextInt()
    for (i in 0..n-1) {
        a[i] = scanner.nextInt()
    }
    val result = two(a, 0, n - 1)
    print("The min number is ${result[0]}\nThe second min number is ${result[1]}")
}

/**
 * 返回一个长度为2的数组，[0]表示最小值, [1]表示第二小值
 */
fun two(data: IntArray, start: Int, end: Int): IntArray {
    if (start == end) {
        return intArrayOf(data[start], data[end])
    } else if (start == end - 1) {
        when {
            data[start] < data[end] -> return intArrayOf(data[start], data[end])
            else -> return intArrayOf(data[end], data[start])
        }
    } else {
        val mid = (start + end) / 2
        val left = two(data, start, mid)
        val right = two(data, mid + 1, end)
        val min: Int
        val secondMin: Int
        // 判断左边和右边4个数中的最小数和次小数
        if (left[0] < right[0]) {
            min = left[0]
            if (left[1] < right[0]) {
                secondMin = left[1]
            } else {
                secondMin = right[0]
            }
        } else {
            min = right[0]
            if (right[1] < left[0]) {
                secondMin = right[1]
            } else {
                secondMin = left[0]
            }
        }
        return intArrayOf(min, secondMin)
    }
}