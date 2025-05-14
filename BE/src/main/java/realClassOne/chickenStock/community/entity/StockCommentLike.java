package realClassOne.chickenStock.community.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;

import java.time.LocalDateTime;

@Entity
@Table(name = "stock_comment_like", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"member_id", "stock_comment_id"})
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class StockCommentLike {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long stockCommentLikeId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "stock_comment_id", nullable = false)
    private StockComment stockComment;

    private LocalDateTime createdAt = LocalDateTime.now();

    public static StockCommentLike of(Member member, StockComment comment) {
        StockCommentLike like = new StockCommentLike();
        like.member = member;
        like.stockComment = comment;
        return like;
    }
}
