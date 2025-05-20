//package realClassOne.chickenStock.stock.trade.controller;
//
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.http.ResponseEntity;
//import org.springframework.security.access.prepost.PreAuthorize;
//import org.springframework.web.bind.annotation.*;
//import realClassOne.chickenStock.member.repository.MemberRepository;
//import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
//import realClassOne.chickenStock.stock.dto.response.CommonResponseDTO;
//import realClassOne.chickenStock.stock.trade.service.TradingSecurityService;
//
//import java.util.Map;
//
//@RestController
//@RequestMapping("/api/admin/trading")
//@RequiredArgsConstructor
//@Slf4j
//public class TradingAdminController {
//
//    private final TradingSecurityService tradingSecurityService;
//    private final JwtTokenProvider jwtTokenProvider;
//    private final MemberRepository memberRepository;
//
//    /**
//     * 회원의 거래 제한 상태 확인
//     */
//    @GetMapping("/restriction/{memberId}")
//    @PreAuthorize("hasRole('ROLE_ADMIN')")
//    public ResponseEntity<Map<String, Object>> checkRestriction(@PathVariable Long memberId) {
//        boolean isRestricted = tradingSecurityService.isRestricted(memberId);
//        return ResponseEntity.ok(Map.of(
//                "memberId", memberId,
//                "restricted", isRestricted
//        ));
//    }
//
//    /**
//     * 거래 제한 해제 API
//     */
//    @PostMapping("/restriction/remove/{memberId}")
//    @PreAuthorize("hasRole('ROLE_ADMIN')")
//    public ResponseEntity<CommonResponseDTO> removeRestriction(@PathVariable Long memberId) {
//        boolean success = tradingSecurityService.removeRestriction(memberId);
//
//        if (success) {
//            log.info("회원 ID {} 거래 제한 해제 완료", memberId);
//            return ResponseEntity.ok(new CommonResponseDTO("success", "거래 제한이 해제되었습니다."));
//        } else {
//            log.warn("회원 ID {} 거래 제한 해제 실패 (제한 상태가 아님)", memberId);
//            return ResponseEntity.ok(new CommonResponseDTO("warning", "회원이 이미 제한 상태가 아닙니다."));
//        }
//    }
//
//    /**
//     * 내 계정 거래 제한 상태 확인
//     */
//    @GetMapping("/my-restriction")
//    public ResponseEntity<Map<String, Object>> checkMyRestriction(@RequestHeader("Authorization") String authorization) {
//        try {
//            String token = jwtTokenProvider.resolveToken(authorization);
//            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
//
//            boolean isRestricted = tradingSecurityService.isRestricted(memberId);
//            return ResponseEntity.ok(Map.of(
//                    "memberId", memberId,
//                    "restricted", isRestricted
//            ));
//        } catch (Exception e) {
//            log.error("내 거래 제한 상태 확인 중 오류 발생", e);
//            return ResponseEntity.badRequest().body(Map.of(
//                    "error", "유효하지 않은 인증 정보입니다."
//            ));
//        }
//    }
//
//    /**
//     * 내 계정 거래 제한 즉시 해제 요청 (일반 회원용)
//     */
//    @PostMapping("/remove-my-restriction")
//    public ResponseEntity<CommonResponseDTO> removeMyRestriction(@RequestHeader("Authorization") String authorization) {
//        try {
//            String token = jwtTokenProvider.resolveToken(authorization);
//            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
//
//            if (!tradingSecurityService.isRestricted(memberId)) {
//                return ResponseEntity.ok(new CommonResponseDTO("warning", "회원님은 현재 거래 제한 상태가 아닙니다."));
//            }
//
//            boolean success = tradingSecurityService.removeRestriction(memberId);
//
//            if (success) {
//                log.info("회원 ID {} 본인 요청으로 거래 제한 해제 완료", memberId);
//                return ResponseEntity.ok(new CommonResponseDTO("success", "거래 제한이 즉시 해제되었습니다."));
//            } else {
//                log.warn("회원 ID {} 본인 요청 거래 제한 해제 실패", memberId);
//                return ResponseEntity.ok(new CommonResponseDTO("error", "거래 제한 해제에 실패했습니다. 고객센터에 문의하세요."));
//            }
//        } catch (Exception e) {
//            log.error("내 거래 제한 해제 중 오류 발생", e);
//            return ResponseEntity.badRequest().body(new CommonResponseDTO("error", "유효하지 않은 인증 정보입니다."));
//        }
//    }
//}