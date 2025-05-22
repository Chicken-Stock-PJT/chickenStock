package realClassOne.chickenStock.rank.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class ReturnRateRankingResponseDTO {
    private List<ReturnRateRankingEntryDTO> topRankings;
    private ReturnRateRankingEntryDTO myRank;
}
