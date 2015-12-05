package e14

import java.util.*

/**
 * Created by yangruihan on 15/12/5.
 */

val board = Array(100, { IntArray(100) })
var amount = 0

fun main(args: Array<String>) {
    val scanner = Scanner(System.`in`)
    val k = scanner.nextInt()
    var size = 1
    for (i in 0..k - 1)
        size *= 2
    println("Input incomplete pane")
    val x = scanner.nextInt()
    val y = scanner.nextInt()
    cover(0, 0, x, y, size)
    for (i in 0..size - 1) {
        for (j in 0..size - 1)
            print("${board[j][i]}\t")
        println()
    }
}

fun cover(tr: Int, tc: Int, dr: Int, dc: Int, size: Int) {
    var size = size
    if (size < 2) return
    amount++
    size /= 2
    // 残缺块在第一象限
    if (dr < tr + size && dc < tc + size) {
        board[tr + size][tc + size - 1] = amount
        board[tr + size - 1][tc + size] = amount
        board[tr + size][tc + size] = amount
        cover(tr, tc, dr, dc, size)
        cover(tr + size, tc, tr + size, tc + size - 1, size) // 继续覆盖第二象限
        cover(tr, tc + size, tr + size - 1, tc + size, size) // 继续覆盖第三象限
        cover(tr + size, tc + size, tr + size, tc + size, size)
    }
    // 残缺块在第二象限
    if (dr >= tr + size && dc < tc + size) {
        board[tr + size - 1][tc + size - 1] = amount
        board[tr + size - 1][tc + size] = amount
        board[tr + size][tc + size] = amount
        cover(tr + size, tc, dr, dc, size)
        cover(tr, tc, tr + size - 1, tc + size - 1, size)
        cover(tr, tc + size, tr + size - 1, tc + size, size)
        cover(tr + size, tc + size, tr + size, tc + size, size)
    }
    // 残缺块在第三象限
    if (dr < tr + size && dc >= tc + size) {
        board[tr + size - 1][tc + size - 1] = amount
        board[tr + size][tc + size - 1] = amount
        board[tr + size][tc + size] = amount
        cover(tr, tc + size, dr, dc, size)
        cover(tr, tc, tr + size - 1, tc + size - 1, size)
        cover(tr + size, tc, tr + size, tc + size - 1, size)
        cover(tr + size, tc + size, tr + size, tc + size, size)
    }
    // 残缺块在第四象限
    if (dr >= tr + size && dc >= tc + size) {
        board[tr + size - 1][tc + size - 1] = amount
        board[tr + size][tc + size - 1] = amount
        board[tr + size - 1][tc + size] = amount
        cover(tr + size, tc + size, dr, dc, size)
        cover(tr, tc, tr + size - 1, tc + size - 1, size)
        cover(tr + size, tc, tr + size, tc + size - 1, size)
        cover(tr, tc + size, tr + size - 1, tc + size, size)
    }
}