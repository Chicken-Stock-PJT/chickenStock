package realClassOne.chickenStock.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class SecurityUtil {

    // JWT에서 memberId(Long)을 꺼내는 기능
    public static Long getCurrentMemberId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated()) {
            throw new RuntimeException("인증 정보가 없습니다.");
        }

        String principal = null;

        if (authentication.getPrincipal() instanceof UserDetails userDetails) {
            principal = userDetails.getUsername(); // subject가 문자열로 저장됨
        } else if (authentication.getPrincipal() instanceof String str) {
            principal = str;
        }

        if (principal == null) {
            throw new RuntimeException("인증된 사용자 정보를 찾을 수 없습니다.");
        }

        try {
            return Long.parseLong(principal); // subject가 memberId이므로 숫자로 변환
        } catch (NumberFormatException e) {
            throw new RuntimeException("memberId 형식이 올바르지 않습니다.");
        }
    }

    public static String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated()) {
            throw new RuntimeException("인증 정보가 없습니다.");
        }

        Object principal = authentication.getPrincipal();

        if (principal instanceof UserDetails) {
            return ((UserDetails) principal).getUsername();
        } else if (principal instanceof String) {
            return (String) principal;
        }

        return null;
    }
}
