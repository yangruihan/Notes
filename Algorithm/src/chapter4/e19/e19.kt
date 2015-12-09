package e19

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

fun judge(data: String, record: IntArray, position: Int): Boolean {
    var p = position + 1
    while (record[p++] != 0) {
    }
    if (data[position] > data[p - 1]) {
        return true
    } else {
        return false
    }
}

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    print("Input a number: ")
    val num = scanner.next()
    print("Input s: ")
    val s = scanner.nextInt()
    var record = IntArray(num.length)
    var deleteCount = 0

    loop@for (i in 1..s) {
        for (j in 0..num.length - 2) {
            if (record[j] == 0 && judge(num, record, j)) {
                record[j] = 1 // 删除当前数
                if ((++deleteCount) >= s) {
                    break@loop
                }
                break
            }
        }
    }

    var i = num.length - 1
    while (deleteCount < s) {
        if (record[i] == 0) {
            record[i] = 1
            deleteCount++
        }
    }

    for (i in 0..num.length - 1) {
        if (record[i] == 0 && !(num[i] == '0' && i != num.length - 1)) {
            print(num[i])
        }
    }

}