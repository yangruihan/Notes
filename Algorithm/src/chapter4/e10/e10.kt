package e10

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    for (i in 1..9)
        for (j in 3..9)
            if ((100000 * i + 10000 * i + 1000 * i + 100 * i + 10 * i + i) % j == 0) {
                val k = (100000 * i + 10000 * i + 1000 * i + 100 * i + 10 * i + i) / j
                if (k < 100000 && k / 10000 % 10 == j && k / 10 % 10 == j && k / 1000 % 10 == k % 10)
                    print("$k * $j = $i$i$i$i$i$i\n")
            }
}