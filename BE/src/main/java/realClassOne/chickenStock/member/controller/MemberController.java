package realClassOne.chickenStock.member.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.member.dto.response.MemberResponseDto;
import realClassOne.chickenStock.member.service.MemberService;

@RestController
@RequestMapping("/api/members")
@RequiredArgsConstructor
public class MemberController {
    // 회원 정보조회/ 수정, 탈퇴, 프로필 관리 등 기타 회원 데이터 관련 API

    private final MemberService memberService;

    @GetMapping("/me")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<MemberResponseDto> getCurrentUser() {
        return ResponseEntity.ok(memberService.getCurrentUser());
    }
}
