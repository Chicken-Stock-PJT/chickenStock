package realClassOne.chickenStock.member.controller;

import lombok.RequiredArgsConstructor;
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
import realClassOne.chickenStock.stock.dto.response.PortfolioResponseDTO;
import realClassOne.chickenStock.stock.service.PortfolioService;
import realClassOne.chickenStock.stock.service.StockTradeService;

@RestController
@RequestMapping("/api/members")
@RequiredArgsConstructor
public class MemberController {

    private final MemberService memberService;
    private final StockTradeService stockTradeService;
    private final PortfolioService portfolioService;

    @GetMapping("/me")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<MemberResponseDto> getCurrentUser() {
        return ResponseEntity.ok(memberService.getCurrentUser());
    }

    // 비밀번호 변경
    @PostMapping("/change-password")
    public ResponseEntity<?> changePassword(
        @RequestBody @Valid PasswordChangeRequestDTO requestDTO,
        @RequestHeader("Authorization") String authorizationHeader) {
        PasswordChangeResponseDTO response = memberService.changePassword(authorizationHeader, requestDTO);

        return ResponseEntity.ok(response);
    }

    // 닉네임 변경
    @PatchMapping("/nickname")
    public ResponseEntity<NicknameChangeResponseDTO> changeNickname(
            @RequestHeader("Authorization") String authorizationHeader,
            @Valid @RequestBody NicknameChangeRequestDTO requestDTO
    ) {
        NicknameChangeResponseDTO response = memberService.changeNickname(authorizationHeader, requestDTO.getNickname());
        return ResponseEntity.ok(response);
    }

    // 회원 기본금 초기화 API (1억)
    @PostMapping("/initialize-money")
    public ResponseEntity<Object> initializeMemberMoney(
            @RequestHeader("Authorization") String authorizationHeader) {

        return ResponseEntity.ok(stockTradeService.initializeMemberMoney(authorizationHeader));
    }

    // 보유 주식 포트폴리오 조회 API
    // REST API로 초기 데이터를 제공하고, 실시간 업데이트는 웹소켓으로 전송
    @GetMapping("/portfolio")
    public ResponseEntity<PortfolioResponseDTO> getPortfolio(
            @RequestHeader("Authorization") String authorizationHeader) {

        PortfolioResponseDTO portfolioDTO = portfolioService.getPortfolio(authorizationHeader);
        return ResponseEntity.ok(portfolioDTO);
    }
}
