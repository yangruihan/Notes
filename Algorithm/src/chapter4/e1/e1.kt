package e1

/**
 * Created by Yrh on 2015/12/4.
 */
fun main(args: Array<String>) {
    var a = IntArray(12)
    a[0] = 1
    a[1] = 1
    for (i in 2..a.size - 1)
        a[i] = a[i - 1] + a[i - 2]
    a.forEach {
        println(it)
    }
}