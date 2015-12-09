package e20

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

const val MAX = 1000
const val MIN = 1001

/**
 * 返回数组的最大值或最小值和它的位置
 */
fun maxomin(data: IntArray, record: IntArray, flag: Int): IntArray {
    var i = 0
    while (record[i++] == 1) {
    }
    var num = data[i - 1]
    var position = i - 1
    for (i in 0..data.size - 1) {
        when {
            record[i] == 0 && ((flag == MAX && num < data[i]) || (flag == MIN && num > data[i])) -> {
                num = data[i]
                position = i
            }
        }
    }
    return intArrayOf(num, position)
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    print("Input n: ")
    val n = scanner.nextInt()
    val a = IntArray(n)
    for (i in 0..n - 1) {
        a[i] = scanner.nextInt()
    }
    var count = n
    var record = IntArray(n)
    record.fill(0)

    var max = a.clone()

    while (count > 1) {
        var max1 = maxomin(max, record, MAX)
        record[max1[1]] = 1
        var max2 = maxomin(max, record, MAX)
        max[max2[1]] = max1[0] * max2[0] + 1
        count--
    }

    var min_num = 0
    for (i in 0..n - 1) {
        if (record[i] == 0) {
            min_num = max[i]
        }
    }

    count = n
    record.fill(0)
    var min = a.clone()
    while (count > 1) {
        var min1 = maxomin(min, record, MIN)
        record[min1[1]] = 1
        var min2 = maxomin(min, record, MIN)
        min[min2[1]] = min1[0] * min2[0] + 1
        count--
    }


    var max_num = 0
    for (i in 0..n - 1) {
        if (record[i] == 0) {
            max_num = min[i]
        }
    }

    print("m=max-min=${max_num - min_num}")
}