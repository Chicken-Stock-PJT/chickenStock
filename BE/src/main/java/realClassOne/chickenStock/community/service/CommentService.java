package realClassOne.chickenStock.community.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.community.dto.request.CommentRequestDTO;
import realClassOne.chickenStock.community.dto.request.CommentUpdateRequestDTO;
import realClassOne.chickenStock.community.dto.response.CommentReplyResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentUpdateResponseDTO;
import realClassOne.chickenStock.community.entity.StockComment;
import realClassOne.chickenStock.community.exception.CommentErrorCode;
import realClassOne.chickenStock.community.repository.StockCommentRepository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.notification.service.NotificationService;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.community.dto.request.CommentReplyRequestDTO;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class CommentService {

    private final MemberRepository memberRepository;
    private final StockCommentRepository commentRepository;
    private final StockDataRepository stockDataRepository;
    private final JwtTokenProvider jwtTokenProvider;
    private final NotificationService notificationService;

    public List<CommentResponseDTO> getCommentsByStock(String shortCode) {
        StockData stock = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        List<StockComment> parentComments = commentRepository
                .findByStockDataStockDataIdAndParentIsNullOrderByCreatedAtDesc(stock.getStockDataId());

        return parentComments.stream()
                .map(parent -> {
                    List<StockComment> children = commentRepository.findByParentIdOrderByCreatedAt(parent.getId());
                    List<CommentResponseDTO> childDTOs = children.stream()
                            .map(child -> CommentResponseDTO.from(child, List.of()))
                            .toList();
                    return CommentResponseDTO.from(parent, childDTOs);
                })
                .toList();
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

        return CommentResponseDTO.from(comment, List.of());
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

        // 부모 댓글 작성자에게 알림 (본인이 아닌 경우)
        if (!parent.getMember().getMemberId().equals(memberId)) {
            notificationService.createCommentNotification(
                    parent.getMember().getMemberId(),
                    stock.getShortName(),
                    member.getNickname(),
                    reply.getId()
            );
        }

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