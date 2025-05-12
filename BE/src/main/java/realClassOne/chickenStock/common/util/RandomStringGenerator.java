package realClassOne.chickenStock.common.util;

import java.util.Random;

public class RandomStringGenerator {

    private static final String CHAR_POOL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    private static final Random RANDOM = new Random();

    /**
     * 주어진 prefix와 총 길이에 맞게 랜덤 문자열을 생성합니다.
     *
     * @param prefix 앞에 붙일 문자열 (예: K, G, N 등)
     * @param totalLength prefix 포함 전체 문자열 길이 (예: 10)
     * @return 생성된 문자열 (ex: K39akz91, Gk1a9X2d 등)
     */
    public static String generateWithPrefix(String prefix, int totalLength) {
        if (prefix == null) prefix = "";
        int randomLength = totalLength - prefix.length();

        if (randomLength < 0) {
            throw new IllegalArgumentException("prefix 길이가 totalLength보다 길 수 없습니다.");
        }

        StringBuilder sb = new StringBuilder(prefix);

        for (int i = 0; i < randomLength; i++) {
            int idx = RANDOM.nextInt(CHAR_POOL.length());
            sb.append(CHAR_POOL.charAt(idx));
        }

        return sb.toString();
    }
}
