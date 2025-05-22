package realClassOne.chickenStock.community.dto.request;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class CommentReplyRequestDTO {
    private String content;
    private Long parentId;
}
