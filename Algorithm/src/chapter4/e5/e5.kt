package e5

/**
 * Created by Yrh on 2015/12/4.
 */
fun main(args: Array<String>) {
    var dis = 500
    var k = 1
    var oil = 500
    do {
        print("storepoint $k distance ${1000 - dis} oilquantity $oil\n")
        k++
        dis += 500 / (2 * k - 1)
        oil = 500 * k
    } while (dis < 1000)
    oil = 500 * (k - 1) + (1000 - dis) * (2 * k - 1)
    print("storepoint $k distance 0 oilquantity $oil\n")
}