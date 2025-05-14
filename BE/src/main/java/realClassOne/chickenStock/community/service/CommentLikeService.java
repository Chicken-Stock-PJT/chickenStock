package realClassOne.chickenStock.community.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.community.entity.StockComment;
import realClassOne.chickenStock.community.entity.StockCommentLike;
import realClassOne.chickenStock.community.exception.CommentErrorCode;
import realClassOne.chickenStock.community.repository.StockCommentLikeRepository;
import realClassOne.chickenStock.community.repository.StockCommentRepository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CommentLikeService {

    private final StockCommentRepository stockCommentRepository;
    private final StockCommentLikeRepository stockCommentLikeRepository;
    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;


    @Transactional
    public boolean toggleLike(String shortCode, Long commentId, String authorizationHeader) {
        // 토큰 파싱
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 🔑 Member 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 댓글 조회
        StockComment comment = stockCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        // 기존 좋아요 있는지 확인
        Optional<StockCommentLike> existing = stockCommentLikeRepository.findByMemberAndStockComment(member, comment);

        if (existing.isPresent()) {
            stockCommentLikeRepository.delete(existing.get());
            return false; // 좋아요 취소됨
        } else {
            stockCommentLikeRepository.save(StockCommentLike.of(member, comment));
            return true; // 좋아요 등록됨
        }
    }

}

