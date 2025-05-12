package realClassOne.chickenStock.member.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.member.dto.request.NicknameChangeRequestDTO;
import realClassOne.chickenStock.member.dto.response.*;
import realClassOne.chickenStock.member.service.MemberService;
import jakarta.validation.Valid;
import realClassOne.chickenStock.member.dto.request.PasswordChangeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.PortfolioResponseDTO;
import realClassOne.chickenStock.stock.service.PortfolioService;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.time.LocalDateTime;

@RestController
@RequestMapping("/api/members")
@RequiredArgsConstructor
public class MemberController {
    // 회원 정보조회/ 수정, 탈퇴, 프로필 관리 등 기타 회원 데이터 관련 API

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

    // 간단 회원 정보 조회
    @GetMapping("/simple-profile")
    public ResponseEntity<SimpleMemberProfileResponseDTO> getSimpleProfile(
            @RequestHeader("Authorization") String authorizationHeader) {
        SimpleMemberProfileResponseDTO responseDTO = memberService.getSimpleProfile(authorizationHeader);
        return ResponseEntity.ok(responseDTO);
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

    // 관심종목 조회 API
    @GetMapping("/watchlist")
    public ResponseEntity<WatchListResponseDTO> getWatchList(
            @RequestHeader("Authorization") String authorizationHeader) {
        WatchListResponseDTO response = memberService.getWatchList(authorizationHeader);
        return ResponseEntity.ok(response);
    }

    // 관심종목 추가 API
    @PostMapping("/watchlist/{stockCode}")
    public ResponseEntity<WatchListResponseDTO> addToWatchList(
            @RequestHeader("Authorization") String authorizationHeader,
            @PathVariable String stockCode) {
        memberService.addToWatchList(authorizationHeader, stockCode);
        return ResponseEntity.ok().build();
    }

    // 관심종목 삭제 API
    @DeleteMapping("/watchlist/{stockCode}")
    public ResponseEntity<WatchListResponseDTO> removeFromWatchList(
            @RequestHeader("Authorization") String authorizationHeader,
            @PathVariable String stockCode) {
        memberService.removeFromWatchList(authorizationHeader, stockCode);
        return ResponseEntity.ok().build();
    }

    // 자산 비중 조회 API
    @GetMapping("/asset-allocation")
    public ResponseEntity<AssetAllocationResponseDTO> getAssetAllocation(
            @RequestHeader("Authorization") String authorizationHeader) {
        AssetAllocationResponseDTO response = memberService.getAssetAllocation(authorizationHeader);
        return ResponseEntity.ok(response);
    }

    // 상세 자산 비중 조회 API
    @GetMapping("/asset-allocation/detail")
    public ResponseEntity<AssetAllocationResponseDTO> getDetailedAssetAllocation(
            @RequestHeader("Authorization") String authorizationHeader) {
        AssetAllocationResponseDTO response = memberService.getDetailedAssetAllocation(authorizationHeader);
        return ResponseEntity.ok(response);
    }

    // 전체 수익률 조회 API
    @GetMapping("/return-rate")
    public ResponseEntity<ReturnRateResponseDTO> getOverallReturnRate(
            @RequestHeader("Authorization") String authorizationHeader) {
        ReturnRateResponseDTO response = memberService.getOverallReturnRate(authorizationHeader);
        return ResponseEntity.ok(response);
    }

    // 기간별 수익률 조회 API
    @GetMapping("/return-rate/period")
    public ResponseEntity<ReturnRateResponseDTO> getPeriodReturnRate(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestParam(required = false, defaultValue = "all") String period) {
        ReturnRateResponseDTO response = memberService.getPeriodReturnRate(authorizationHeader, period);
        return ResponseEntity.ok(response);
    }

    // 특정 종목 거래내역 조회 API
    @GetMapping("/trade-history/{stockCode}")
    public ResponseEntity<StockTradeHistoryResponseDTO> getStockTradeHistory(
            @RequestHeader("Authorization") String authorizationHeader,
            @PathVariable String stockCode) {

        StockTradeHistoryResponseDTO response = memberService.getStockTradeHistory(
                authorizationHeader, stockCode);
        return ResponseEntity.ok(response);
    }

    // 보유 중인 주식 조회 API
    @GetMapping("/holding-stocks")
    public ResponseEntity<HoldingStocksResponseDTO> getHoldingStocks(
            @RequestHeader("Authorization") String authorizationHeader) {

        HoldingStocksResponseDTO response = memberService.getHoldingStocks(authorizationHeader);
        return ResponseEntity.ok(response);
    }
}
