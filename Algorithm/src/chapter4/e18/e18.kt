package e18

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = IntArray(100)
    print("Input n: ")
    val n = scanner.nextInt()
    print("Input k: ")
    val k = scanner.nextInt()
    for (i in 0..n - 1) {
        a[i] = scanner.nextInt()
    }
    print("The ${k}th min number is ${find(a, 0, n - 1, k)}")
}

fun find(data: IntArray, start: Int, end: Int, k: Int): Int {

    if (start > end) return data[start]

    val flag = data[start]
    var s = start
    var e = end
    while (s < e) {
        while (s < e && data[e] >= flag) {
            e--
        }
        data[s] = data[e]
        while (s < e && data[s] <= flag) {
            s++
        }
        data[e] = data[s]
    }
    data[s] = flag
    when {
        // 如果 s > k 说明第k小值在左半部分
        s > k - 1 -> return find(data, start, s - 1, k)
        // 如果 s < k 说明第k小值在右半部分
        s < k - 1 -> return find(data, s + 1, end, k - s)
        s == 0 && e == 0 -> return data[k - 1]
        else -> return data[s + start - 1]
    }
}
