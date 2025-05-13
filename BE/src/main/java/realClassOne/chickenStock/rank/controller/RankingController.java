package realClassOne.chickenStock.rank.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.rank.dto.response.RankingEntryDTO;
import realClassOne.chickenStock.rank.dto.response.RankingResponseDTO;
import realClassOne.chickenStock.rank.service.RankingService;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.*;

@RestController
@RequestMapping("/api/ranking")
@RequiredArgsConstructor
public class RankingController {

    private final RankingService rankingService;

    @GetMapping("/total-asset")
    public ResponseEntity<RankingResponseDTO> getTotalAssetRanking(
            @RequestHeader(value = "Authorization", required = false) String authorizationHeader) {
        return ResponseEntity.ok(rankingService.getTop100WithMyRank(authorizationHeader));
    }

    // ai 별도 랭킹 조회
    @GetMapping("/total-asset/ai")
    public ResponseEntity<List<RankingEntryDTO>> getCustomGroupRanking() {
        return ResponseEntity.ok(rankingService.getGroupMembersRankInGlobal(List.of(1L, 2L, 3L, 4L)));
    }
}
