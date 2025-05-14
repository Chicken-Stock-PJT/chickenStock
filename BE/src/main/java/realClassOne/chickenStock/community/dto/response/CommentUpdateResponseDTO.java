package realClassOne.chickenStock.community.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class CommentUpdateResponseDTO {
    private Long id;
    private String content;
    private String nickname;
    private String updatedAt;
}
