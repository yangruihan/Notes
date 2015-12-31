package e21

import java.util.*

/**
 * Created by Yrh on 2015/12/9.
 */

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)

    print("Input a proper fraction A/B: ")
    var a = scanner.nextInt()
    var b = scanner.nextInt()

    print("$a/$b = ")

    loop@ while (true) {
        when {
            a == 1 -> {
                print("$a/$b")
                break@loop
            }
            a > 1 && b % a == 0 -> {
                print("1/${b / a}")
                break@loop
            }
            else -> {
                val c = b / a + 1
                print("1/$c + ")
                a = a * c - b
                b *= c
            }
        }

    }

}