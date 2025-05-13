package realClassOne.chickenStock.rank.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class RankingResponseDTO {
    private List<RankingEntryDTO> topRankings;
    private RankingEntryDTO myRank; // 비로그인 유저면 null
}
