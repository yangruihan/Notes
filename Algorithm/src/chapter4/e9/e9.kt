package e9

/**
 * Created by yangruihan on 15/12/5.
 */

fun main(args: Array<String>) {
    for (i in 1..20)
        for (j in 1..33) {
            val k = 100 - i - j
            if (k % 3 == 0 && 5 * i + 3 * j + k / 3 == 100)
                print(" The cock number is $i\n The hen number is $j\n The chick number is $k\n")
        }
}