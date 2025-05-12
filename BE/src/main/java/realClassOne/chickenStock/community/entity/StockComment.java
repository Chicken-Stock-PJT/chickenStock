package realClassOne.chickenStock.community.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.StockData;

import java.time.LocalDateTime;

@Entity
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class StockComment {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // 종목 정보 (Foreign Key)
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "stock_data_id")
    private StockData stockData;

    // 작성자
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id")
    private Member member;

    // 댓글 내용
    private String content;

    // 부모 댓글 (대댓글용)
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "parent_id")
    private StockComment parent;

    // 삭제 여부
    private boolean isDeleted = false;

    // 생성/수정 시간
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // 생성 메서드
    public static StockComment of(StockData stockData, Member member, String content, StockComment parent) {
        StockComment comment = new StockComment();
        comment.stockData = stockData;
        comment.member = member;
        comment.content = content;
        comment.parent = parent;
        comment.createdAt = LocalDateTime.now();
        comment.updatedAt = LocalDateTime.now();
        return comment;
    }

    // 수정 메서드
    public void updateContent(String content) {
        this.content = content;
        this.updatedAt = LocalDateTime.now();
    }

    // 삭제 메서드
    public void deleteContent() {
        this.isDeleted = true;
        this.updatedAt = LocalDateTime.now(); // 삭제도 수정의 일환
    }
}