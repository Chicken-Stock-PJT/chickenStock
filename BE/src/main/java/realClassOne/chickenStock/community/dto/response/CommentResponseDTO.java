package realClassOne.chickenStock.community.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;
import realClassOne.chickenStock.community.entity.StockComment;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@AllArgsConstructor
public class CommentResponseDTO {
    private Long id;
    private String content;
    private String nickname;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private boolean isDeleted;
    private List<CommentResponseDTO> children;

    public static CommentResponseDTO from(StockComment comment, List<CommentResponseDTO> children) {
        return new CommentResponseDTO(
                comment.getId(),
                comment.getContent(),
                comment.getMember().getNickname(),
                comment.getCreatedAt(),
                comment.getUpdatedAt(),
                comment.isDeleted(),
                children
        );
    }
}

