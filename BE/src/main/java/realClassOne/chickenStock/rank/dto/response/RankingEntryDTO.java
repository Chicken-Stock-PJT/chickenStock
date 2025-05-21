package realClassOne.chickenStock.rank.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class RankingEntryDTO {
    private int rank;
    private String nickname;
    private Long totalAsset;
    private Long memberId;
}