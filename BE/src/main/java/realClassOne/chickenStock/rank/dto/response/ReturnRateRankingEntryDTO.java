package realClassOne.chickenStock.rank.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class ReturnRateRankingEntryDTO {
    private int rank;
    private String nickname;
    private double returnRate;
}

