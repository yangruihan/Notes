public class KMPTest {

    // 通过计算返回串 str 的 next 数组
    public static int[] getNext(String str) {
        int[] next = new int[str.length()];

        next[0] = 0;
        int i = 0;
        int j = -1;

        while (i < str.length() - 1) {
            if (j == -1 || str.charAt(i) == str.charAt(j)) {
                ++j;
                ++i;
                if (str.charAt(i) == str.charAt(j)) {
                    next[i] = next[j];
                } else {
                    next[i] = j + 1;
                }
            } else {
                j = next[j] - 1;
            }
        }

        return next;
    }

    // 返回子串 sub 在主串 str 中第 pos 个字符后的位置。若不存在返回0
    public static int indexKMP(String str, String sub, int pos) {
        int i = pos - 1; // i 用于主串 str 当前位置下标值
        int j = -1; // j 用于子串 sub 当前位置下标值

        // 得到子串 sub 的 next 数组
        int[] next = getNext(sub);

        while (i < str.length() && j < sub.length()) {
            if (j == -1 || str.charAt(i) == sub.charAt(j)) {
                ++i;
                ++j;
            } else {
                j = next[j] - 1;
            }
        }

        if (j >= sub.length()) {
            return i - sub.length();
        } else {
            return 0;
        }
    }

    public static void main(String[] args) {

        String str = "abcabbaba";
        String sub = "aba";
        System.out.println(indexKMP(str, sub, 0));

        str = "ldsfjlasjldfjassfsadf";
        sub = "ldfja";
        System.out.println(indexKMP(str, sub, 0));

        str = "dfsadfsadfsdaflkasdf";
        sub = "dfs";
        System.out.println(indexKMP(str, sub, 5));
    }
}