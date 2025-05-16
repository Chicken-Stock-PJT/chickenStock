package realClassOne.chickenStock.rank.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.rank.dto.response.RankingEntryDTO;
import realClassOne.chickenStock.rank.dto.response.RankingResponseDTO;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RankingService {

    private final ZSetOperations<String, String> zSetOperations;
    private final MemberRepository memberRepository;
    private final JwtTokenProvider jwtTokenProvider;

    @Autowired
    private final TotalAssetCalculator totalAssetCalculator;

    private static final String REDIS_KEY = "ranking:totalAsset";

    // TOP 100 랭킹과 로그인한 사용자의 공동 순위 정보를 반환
    public RankingResponseDTO getTop100WithMyRank(String authorizationHeader) {
        Set<ZSetOperations.TypedTuple<String>> topSet =
                zSetOperations.reverseRangeWithScores(REDIS_KEY, 0, -1);  // 전체 순위 조회

        if (topSet == null || topSet.isEmpty()) {
            return new RankingResponseDTO(List.of(), null);
        }

        // 닉네임 캐싱: memberId → nickname
        Map<Long, String> nicknameMap = memberRepository.findAll().stream()
                .collect(Collectors.toMap(Member::getMemberId, Member::getNickname));

        List<RankingEntryDTO> topRankings = new ArrayList<>();
        RankingEntryDTO myRank = null;

        int rank = 0;
        long prevScore = -1;
        int skip = 1;

        Long myId = null;
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            try {
                String token = jwtTokenProvider.resolveToken(authorizationHeader);
                myId = jwtTokenProvider.getMemberIdFromToken(token);
            } catch (Exception ignored) {}
        }

        for (ZSetOperations.TypedTuple<String> tuple : topSet) {
            Long memberId = Long.valueOf(tuple.getValue());
            long totalAsset = tuple.getScore().longValue();

            if (totalAsset != prevScore) {
                rank += skip;
                skip = 1;
            } else {
                skip++;
            }

            String nickname = nicknameMap.getOrDefault(memberId, "탈퇴회원");

            // TOP 100만 담기
            if (topRankings.size() < 100) {
                topRankings.add(new RankingEntryDTO(rank, nickname, totalAsset));
            }

            // 로그인 유저 순위 기록
            if (myId != null && myId.equals(memberId)) {
                myRank = new RankingEntryDTO(rank, nickname, totalAsset);
            }

            prevScore = totalAsset;
        }

        return new RankingResponseDTO(topRankings, myRank);
    }

    // ai 그룹 랭킹 조회
    public List<RankingEntryDTO> getGroupMembersRankInGlobal(List<Long> memberIds) {
        Set<ZSetOperations.TypedTuple<String>> topSet =
                zSetOperations.reverseRangeWithScores(REDIS_KEY, 0, -1);  // 전체 조회

        if (topSet == null || topSet.isEmpty()) {
            totalAssetCalculator.recalculateAll();  // 직접 계산해서 Redis 채우기
            topSet = zSetOperations.reverseRangeWithScores(REDIS_KEY, 0, -1);
        }

        Map<Long, String> nicknameMap = memberRepository.findAllById(memberIds).stream()
                .collect(Collectors.toMap(Member::getMemberId, Member::getNickname));

        List<RankingEntryDTO> filteredRanks = new ArrayList<>();

        int rank = 0;
        long prevScore = -1;
        int skip = 1;

        for (ZSetOperations.TypedTuple<String> tuple : topSet) {
            Long memberId = Long.valueOf(tuple.getValue());
            long totalAsset = tuple.getScore().longValue();

            // 공동 순위 처리
            if (totalAsset != prevScore) {
                rank += skip;
                skip = 1;
            } else {
                skip++;
            }

            // 대상 멤버만 필터링
            if (memberIds.contains(memberId)) {
                String nickname = nicknameMap.getOrDefault(memberId, "탈퇴회원");
                filteredRanks.add(new RankingEntryDTO(rank, nickname, totalAsset));
            }

            prevScore = totalAsset;
        }

        return filteredRanks;
    }
}
