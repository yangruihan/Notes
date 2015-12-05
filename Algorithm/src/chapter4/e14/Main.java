package e14;

import java.util.Scanner;

/**
 * Created by yangruihan on 15/12/5.
 */
public class Main {

    static int amount = 0;
    static int[][] board = new int[100][100];

    public static void main(String argv[]) {
        Scanner scanner = new Scanner(System.in);
        int k = scanner.nextInt();
        int size = 1;
        for (int i = 0; i < k; i++)
            size *= 2;
        System.out.println("Input incomplete pane");
        int x = scanner.nextInt();
        int y = scanner.nextInt();
        cover(0, 0, x, y, size);
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++)
                System.out.print(board[j][i] + "\t");
            System.out.println();
        }
    }

    public static void cover(int tr, int tc, int dr, int dc, int size) {
        if (size < 2) return;
        amount++;
        size /= 2;
        // 残缺块在第一象限
        if (dr < tr + size && dc < tc + size) {
            board[tr + size][tc + size - 1] = amount;
            board[tr + size - 1][tc + size] = amount;
            board[tr + size][tc + size] = amount;
            cover(tr, tc, dr, dc, size);
            cover(tr + size, tc, tr + size, tc + size - 1, size); // 继续覆盖第二象限
            cover(tr, tc + size, tr + size - 1, tc + size, size); // 继续覆盖第三象限
            cover(tr + size, tc + size, tr + size, tc + size, size);
        }
        // 残缺块在第二象限
        if (dr >= tr + size && dc < tc + size) {
            board[tr + size - 1][tc + size - 1] = amount;
            board[tr + size - 1][tc + size] = amount;
            board[tr + size][tc + size] = amount;
            cover(tr + size, tc, dr, dc, size);
            cover(tr, tc, tr + size - 1, tc + size - 1, size);
            cover(tr, tc + size, tr + size - 1, tc + size, size);
            cover(tr + size, tc + size, tr + size, tc + size, size);
        }
        // 残缺块在第三象限
        if (dr < tr + size && dc >= tc + size) {
            board[tr + size - 1][tc + size - 1] = amount;
            board[tr + size][tc + size - 1] = amount;
            board[tr + size][tc + size] = amount;
            cover(tr, tc + size, dr, dc, size);
            cover(tr, tc, tr + size - 1, tc + size - 1, size);
            cover(tr + size, tc, tr + size, tc + size - 1, size);
            cover(tr + size, tc + size, tr + size, tc + size, size);
        }
        // 残缺块在第四象限
        if (dr >= tr + size && dc >= tc + size) {
            board[tr + size - 1][tc + size - 1] = amount;
            board[tr + size][tc + size - 1] = amount;
            board[tr + size - 1][tc + size] = amount;
            cover(tr + size, tc + size, dr, dc, size);
            cover(tr, tc, tr + size - 1, tc + size - 1, size);
            cover(tr + size, tc, tr + size, tc + size - 1, size);
            cover(tr, tc + size, tr + size - 1, tc + size, size);
        }
    }
}
