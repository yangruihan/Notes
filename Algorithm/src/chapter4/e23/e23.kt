package e23

import java.util.*

/**
 * Created by Yrh on 2015/12/12.
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)

    print("Input n: ")
    val n = scanner.nextInt()

    var a = IntArray(2 * n)
    var oddSum = 0 ;
    var evenSum = 0;
    for (i in 0..2 * n - 1) {
        a[i] = scanner.nextInt()
        when (a[i] % 2) {
            0 -> evenSum += a[i]
            1 -> oddSum += a[i]
        }
    }

    when {
        evenSum > oddSum -> print("第一次取最后那个数")
        evenSum < oddSum -> print("第一次取第一个数")
        else -> print("随便取哪边的数都行")
    }
}