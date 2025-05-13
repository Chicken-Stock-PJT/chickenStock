package realClassOne.chickenStock.rank.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.rank.dto.response.RankingEntryDTO;
import realClassOne.chickenStock.rank.dto.response.RankingResponseDTO;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.*;

@Service
@RequiredArgsConstructor
public class RankingService {

    private final ZSetOperations<String, String> zSetOperations;
    private final MemberRepository memberRepository;
    private final JwtTokenProvider jwtTokenProvider;

    private static final String REDIS_KEY = "ranking:totalAsset";

    public RankingResponseDTO getTop100WithMyRank(String authorizationHeader) {
        Set<ZSetOperations.TypedTuple<String>> topSet =
                zSetOperations.reverseRangeWithScores(REDIS_KEY, 0, 99);

        List<RankingEntryDTO> topRankings = new ArrayList<>();
        int rank = 1;
        long prevScore = -1;
        int sameCount = 0;

        for (ZSetOperations.TypedTuple<String> tuple : topSet) {
            Long memberId = Long.valueOf(tuple.getValue());
            long totalAsset = tuple.getScore().longValue();
            String nickname = memberRepository.findById(memberId)
                    .map(Member::getNickname)
                    .orElse("탈퇴회원");

            if (totalAsset == prevScore) {
                sameCount++;
            } else {
                rank += sameCount;
                sameCount = 1;
            }

            topRankings.add(new RankingEntryDTO(rank, nickname, totalAsset));
            prevScore = totalAsset;
        }

        // 로그인 유저 등수 조회
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

        return new RankingResponseDTO(topRankings, myRank);
    }
}