package realClassOne.chickenStock.community.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.community.dto.request.CommentRequestDTO;
import realClassOne.chickenStock.community.dto.request.CommentReplyRequestDTO;
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
import realClassOne.chickenStock.notification.service.NotificationService;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

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
    private final NotificationService notificationService;  // 추가

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
            nextCursor = lastCreatedAt.toString();
        }

        return new CommentPageResponseDTO(responseDTOs, nextCursor);
    }

    @Transactional
    public CommentResponseDTO createComment(String shortCode, CommentRequestDTO dto, String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        StockComment comment = StockComment.of(stock, member, dto.getContent(), null);
        commentRepository.save(comment);

        return CommentResponseDTO.from(comment, List.of(), 0L, false);
    }

    // 대댓글 생성 - 알림 기능 추가
    @Transactional
    public CommentReplyResponseDTO createReply(String shortCode, CommentReplyRequestDTO dto, String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        StockComment parent = commentRepository.findById(dto.getParentId())
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        StockComment reply = StockComment.of(stock, member, dto.getContent(), parent);
        commentRepository.save(reply);

        // 부모 댓글 작성자에게 알림 발송 (본인이 아닌 경우)
        if (!parent.getMember().getMemberId().equals(memberId)) {
            notificationService.createCommentNotification(
                    parent.getMember().getMemberId(),  // 알림 받을 사람
                    memberId,                          // 알림 보내는 사람
                    stock.getShortName(),
                    member.getNickname(),
                    reply.getId()
            );
        }

        return CommentReplyResponseDTO.from(reply);
    }

    @Transactional
    public CommentUpdateResponseDTO updateComment(Long commentId, CommentUpdateRequestDTO dto, String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        StockComment comment = commentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(CommentErrorCode.COMMENT_NOT_FOUND));

        if (!comment.getMember().getMemberId().equals(memberId)) {
            throw new CustomException(CommentErrorCode.NOT_AUTHOR);
        }

        comment.updateContent(dto.getContent());

        return new CommentUpdateResponseDTO(
                comment.getId(),
                comment.getContent(),
                comment.getMember().getNickname(),
                comment.getUpdatedAt().toString()
        );
    }

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

        comment.deleteContent();
    }
}