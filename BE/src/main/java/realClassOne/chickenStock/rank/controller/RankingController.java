package realClassOne.chickenStock.rank.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.rank.dto.response.RankingEntryDTO;
import realClassOne.chickenStock.rank.dto.response.RankingResponseDTO;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.*;

@RestController
@RequestMapping("/api/rankings")
@RequiredArgsConstructor
public class RankingController {

    private final ZSetOperations<String, String> zSetOperations;
    private final MemberRepository memberRepository;
    private final JwtTokenProvider jwtTokenProvider;

    private static final String REDIS_KEY = "ranking:totalAsset";

    @GetMapping("/valuation")
    public ResponseEntity<RankingResponseDTO> getTop100WithMyRank(
            @RequestHeader(value = "Authorization", required = false) String authorizationHeader) {

        Set<ZSetOperations.TypedTuple<String>> topSet =
                zSetOperations.reverseRangeWithScores(REDIS_KEY, 0, 99);

        List<RankingEntryDTO> topRankings = new ArrayList<>();
        int rank = 1;

        for (ZSetOperations.TypedTuple<String> tuple : topSet) {
            Long memberId = Long.valueOf(tuple.getValue());
            Double totalAsset = tuple.getScore();
            String nickname = memberRepository.findById(memberId)
                    .map(Member::getNickname)
                    .orElse("탈퇴회원");

            topRankings.add(new RankingEntryDTO(rank++, nickname, totalAsset.longValue()));
        }

        // 로그인 유저의 등수 계산 (비로그인 시 null)
        RankingEntryDTO myRank = null;

        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            try {
                String token = jwtTokenProvider.resolveToken(authorizationHeader);
                Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

                Long rankIndex = zSetOperations.reverseRank(REDIS_KEY, memberId.toString());
                Double score = zSetOperations.score(REDIS_KEY, memberId.toString());

                if (rankIndex != null && score != null) {
                    String nickname = memberRepository.findById(memberId)
                            .map(Member::getNickname)
                            .orElse("탈퇴회원");
                    myRank = new RankingEntryDTO(rankIndex.intValue() + 1, nickname, score.longValue());
                }
            } catch (Exception e) {
                // 토큰 파싱 실패 시 무시하고 myRank는 null 유지
            }
        }

        return ResponseEntity.ok(new RankingResponseDTO(topRankings, myRank));
    }
}
