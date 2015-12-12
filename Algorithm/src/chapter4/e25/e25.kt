package e25

import java.util.*

/**
 * Created by Yrh on 2015/12/12.
 */
fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    var a = Array(100, { IntArray(100) })
    var gain = IntArray(100)
    var q = FloatArray(100)
    var f = FloatArray(100)
    var temp = FloatArray(100)

    print("Input the number of item: ")
    val m = scanner.nextInt()

    print("Input how much money: ")
    val n = scanner.nextInt()

    print("Input one item gain table: ")
    for (j in 1..n) {
        q[j] = scanner.nextFloat()
        f[j] = q[j]
    }

    for (j in 1..n) {
        a[1][j] = j
    }

    for (k in 2..m) {
        print("Input another item gain table: ")
        for (j in 1..n) {
            temp[j] = q[j]
            q[j] = scanner.nextFloat()
            a[k][j] = 0
        }

        for (j in 0..n) {
            for (i in 0..j) {
                if (f[j - i] + q[i] > temp[j]) {
                    temp[j] = f[j - i] + q[i]
                    a[k][j] = i
                }
            }
        }

        for (j in 0..n) {
            f[j] = temp[j]
        }
    }

    var rest = n
    for (i in m downTo  1) {
        gain[i] = a[i][rest]
        rest -= gain[i]
    }

    for (i in 1..m) {
        print("${gain[i]} ")
    }

    print(f[n]);

}