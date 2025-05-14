package realClassOne.chickenStock.community.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;
import realClassOne.chickenStock.community.entity.StockComment;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor
public class CommentReplyResponseDTO {
    private Long id;
    private String content;
    private String nickname;
    private LocalDateTime createdAt;
    private Long parentId; // 어떤 댓글에 달린 대댓글인지

    // 정적 팩토리 메서드 추가
    public static CommentReplyResponseDTO from(StockComment comment) {
        Long parentId = comment.getParent() != null ? comment.getParent().getId() : null;

        return new CommentReplyResponseDTO(
                comment.getId(),
                comment.getContent(),
                comment.getMember().getNickname(),
                comment.getCreatedAt(),
                parentId
        );
    }
}
