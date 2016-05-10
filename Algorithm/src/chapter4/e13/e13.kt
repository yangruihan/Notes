package e13

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

var a = FloatArray(100)

fun maxmin(i: Int, j: Int): FloatArray {
    when (i) {
        j -> return floatArrayOf(a[i], a[i])
        j - 1 -> {
            when {
                a[i] < a[j] -> return floatArrayOf(a[j], a[i])
                else -> return floatArrayOf(a[i], a[j])
            }
        }
        else -> {
            var max = 0f
            var min = 0f
            var mid = (i + j) / 2
            val lmaxmin = maxmin(i, mid)
            val rmaxmin = maxmin(mid + 1, j)
            when {
                lmaxmin[0] > rmaxmin[0] -> max = lmaxmin[0]
                else -> max = rmaxmin[0]
            }
            when {
                lmaxmin[1] < rmaxmin[1] -> min = lmaxmin[1]
                else -> min = rmaxmin[1]
            }
            return floatArrayOf(max, min)
        }
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    for (i in 0..n - 1)
        a[i] = scanner.nextFloat()
    print("The max number is ${maxmin(0, n - 1)[0]}\nThe min number is ${maxmin(0, n - 1)[1]}")
}