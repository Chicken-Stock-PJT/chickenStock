package realClassOne.chickenStock.community.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class CommentPageResponseDTO {
    private List<CommentResponseDTO> comments;
    private String nextCursor;  // createdAt 기준 다음 페이지를 위한 커서
}
