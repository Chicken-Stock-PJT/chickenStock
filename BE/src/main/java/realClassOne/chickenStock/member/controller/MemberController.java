package realClassOne.chickenStock.member.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.member.dto.request.NicknameChangeRequestDTO;
import realClassOne.chickenStock.member.dto.response.MemberResponseDto;
import realClassOne.chickenStock.member.dto.response.NicknameChangeResponseDTO;
import realClassOne.chickenStock.member.dto.response.PasswordChangeResponseDTO;
import realClassOne.chickenStock.member.service.MemberService;
import jakarta.validation.Valid;
import realClassOne.chickenStock.member.dto.request.PasswordChangeRequestDTO;

@Slf4j
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

    // 비밀번호 변경
    @PostMapping("/change-password")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<?> changePassword(
        @RequestBody @Valid PasswordChangeRequestDTO requestDTO,
        @RequestHeader("Authorization") String authorizationHeader) {
        memberService.changePassword(authorizationHeader, requestDTO);

        PasswordChangeResponseDTO responseDTO = new PasswordChangeResponseDTO("비밀번호가 성공적으로 변경되었습니다.");
        return ResponseEntity.ok(responseDTO);
    }

    // 닉네임 변경
    @PatchMapping("/nickname")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<NicknameChangeResponseDTO> changeNickname(
            @RequestHeader("Authorization") String authorizationHeader,
            @Valid @RequestBody NicknameChangeRequestDTO requestDTO
    ) {
        memberService.changeNickname(authorizationHeader, requestDTO.getNickname());
        return ResponseEntity.ok(NicknameChangeResponseDTO.of("닉네임이 성공적으로 변경되었습니다."));
    }


}
