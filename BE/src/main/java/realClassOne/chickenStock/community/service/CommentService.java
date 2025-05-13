package realClassOne.chickenStock.community.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.community.dto.request.CommentRequestDTO;
import realClassOne.chickenStock.community.dto.request.CommentUpdateRequestDTO;
import realClassOne.chickenStock.community.dto.response.CommentPageResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentReplyResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentUpdateResponseDTO;
import realClassOne.chickenStock.community.entity.StockComment;
import realClassOne.chickenStock.community.exception.CommentErrorCode;
import realClassOne.chickenStock.community.repository.StockCommentLikeRepository;
import realClassOne.chickenStock.community.repository.StockCommentRepository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.community.dto.request.CommentReplyRequestDTO;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class CommentService {

    private final MemberRepository memberRepository;
    private final StockCommentRepository commentRepository;
    private final StockDataRepository stockDataRepository;
    private final JwtTokenProvider jwtTokenProvider;
    private final StockCommentLikeRepository stockCommentLikeRepository;


    public CommentPageResponseDTO getCommentsByStock(String shortCode, String cursor, int limit, String authorizationHeader) {
        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        Long memberId = null;
        Member member = null;

        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            memberId = jwtTokenProvider.getMemberIdFromToken(token);
            member = memberRepository.findById(memberId).orElse(null);
        }

        final Member currentMember = member;
        LocalDateTime cursorTime = (cursor != null)
                ? LocalDateTime.parse(cursor)
                : LocalDateTime.now();

        List<StockComment> parentComments = commentRepository
                .findByStockDataStockDataIdAndParentIsNullAndCreatedAtLessThanOrderByCreatedAtDesc(
                        stock.getStockDataId(), cursorTime, PageRequest.of(0, limit)
                );

        List<CommentResponseDTO> responseDTOs = parentComments.stream()
                .map(parent -> {
                    List<StockComment> children = commentRepository.findByParentIdOrderByCreatedAt(parent.getId());

                    List<CommentResponseDTO> childDTOs = children.stream()
                            .map(child -> {
                                long likeCount = stockCommentLikeRepository.countByStockComment_Id(child.getId());
                                boolean likedByMe = currentMember != null &&
                                        stockCommentLikeRepository.findByMemberAndStockComment(currentMember, child).isPresent();
                                return CommentResponseDTO.from(child, List.of(), likeCount, likedByMe);
                            })
                            .toList();

                    long likeCount = stockCommentLikeRepository.countByStockComment_Id(parent.getId());
                    boolean likedByMe = currentMember != null &&
                            stockCommentLikeRepository.findByMemberAndStockComment(currentMember, parent).isPresent();

                    return CommentResponseDTO.from(parent, childDTOs, likeCount, likedByMe);
                })
                .toList();

        String nextCursor = null;
        if (!parentComments.isEmpty()) {
            LocalDateTime lastCreatedAt = parentComments.get(parentComments.size() - 1).getCreatedAt();
            nextCursor = lastCreatedAt.toString(); // ISO-8601
        }

        return new CommentPageResponseDTO(responseDTOs, nextCursor);
    }


    public CommentResponseDTO createComment(String shortCode, CommentRequestDTO dto, String authorizationHeader) {
        // JWT에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // DB에서 Member 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        StockComment comment = StockComment.of(stock, member, dto.getContent(), null);
        commentRepository.save(comment);

        return CommentResponseDTO.from(comment, List.of(), 0L, false);
    }

    // 대댓글 생성
    @Transactional
    public CommentReplyResponseDTO createReply(String shortCode, CommentReplyRequestDTO dto, String authorizationHeader) {
        // JWT에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // DB에서 Member 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 종목(게시글 역할)을 shortCode로 조회
        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        // 부모 댓글이 존재하는지 확인
        StockComment parent = commentRepository.findById(dto.getParentId())
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        // 대댓글 생성 (parent 포함)
        StockComment reply = StockComment.of(stock, member, dto.getContent(), parent);
        commentRepository.save(reply);

        // 대댓글 응답 반환
        return CommentReplyResponseDTO.from(reply);
    }


    // 댓글, 대댓글 수정
    @Transactional
    public CommentUpdateResponseDTO updateComment(Long commentId, CommentUpdateRequestDTO dto, String authorizationHeader) {
        // 토큰에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 댓글 조회
        StockComment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        // 작성자 본인인지 확인
        if (!comment.getMember().getMemberId().equals(memberId)) {
            throw new CustomException(CommentErrorCode.NOT_AUTHOR);
        }

        // 내용 수정
        comment.updateContent(dto.getContent());

        // 응답 반환
        return new CommentUpdateResponseDTO(
                comment.getId(),
                comment.getContent(),
                comment.getMember().getNickname(),
                comment.getUpdatedAt().toString()
        );
    }

    // 댓글, 대댓글 삭제
    @Transactional
    public void deleteComment(String shortCode, Long commentId, String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        StockComment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        if (!comment.getStockData().equals(stock)) {
            throw new CustomException(CommentErrorCode.COMMENT_STOCK_MISMATCH);
        }

        if (!comment.getMember().getMemberId().equals(memberId)) {
            throw new CustomException(CommentErrorCode.UNAUTHORIZED_COMMENT_DELETE);
        }

        // soft delete
        comment.deleteContent();
    }


}