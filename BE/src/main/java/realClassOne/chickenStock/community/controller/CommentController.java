package realClassOne.chickenStock.community.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.community.dto.request.CommentReplyRequestDTO;
import realClassOne.chickenStock.community.dto.request.CommentRequestDTO;
import realClassOne.chickenStock.community.dto.request.CommentUpdateRequestDTO;
import realClassOne.chickenStock.community.dto.response.CommentReplyResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentResponseDTO;
import realClassOne.chickenStock.community.dto.response.CommentUpdateResponseDTO;
import realClassOne.chickenStock.community.service.CommentService;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.community.service.CommentLikeService;
import realClassOne.chickenStock.community.repository.StockCommentLikeRepository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/stocks/{shortCode}/comments")
@RequiredArgsConstructor
public class CommentController {

    private final CommentService commentService;
    private final CommentLikeService commentLikeService;
    private final StockCommentLikeRepository stockCommentLikeRepository;

    // 댓글,대댓글 조회
    @GetMapping
    public ResponseEntity<List<CommentResponseDTO>> getComments(@PathVariable String shortCode) {
        List<CommentResponseDTO> comments = commentService.getCommentsByStock(shortCode);
        return ResponseEntity.ok(comments);
    }

    // 댓글 작성
    @PostMapping
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<CommentResponseDTO> createComment(
            @PathVariable String shortCode,
            @RequestBody CommentRequestDTO dto,
            @RequestHeader("Authorization") String authorizationHeader
    ) {
        // 댓글 작성 처리
        CommentResponseDTO responseDTO = commentService.createComment(shortCode, dto, authorizationHeader);
        return ResponseEntity.status(HttpStatus.CREATED).body(responseDTO);
    }


    // 대댓글 작성
    @PostMapping("/reply")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<CommentReplyResponseDTO> createReply(
            @PathVariable String shortCode,
            @RequestBody CommentReplyRequestDTO dto,
            @RequestHeader("Authorization") String authorizationHeader
    ) {
        CommentReplyResponseDTO response = commentService.createReply(shortCode, dto, authorizationHeader);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }


    // 댓글, 대댓글 수정
    @PutMapping("/{commentId}")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<CommentUpdateResponseDTO> updateComment(
            @PathVariable("shortCode") String shortCode,
            @PathVariable Long commentId,
            @RequestBody CommentUpdateRequestDTO dto,
            @RequestHeader("Authorization") String authorizationHeader
    ) {
        CommentUpdateResponseDTO updated = commentService.updateComment(commentId, dto, authorizationHeader);
        return ResponseEntity.ok(updated);
    }

    // 댓글, 대댓글 삭제
    @PreAuthorize("isAuthenticated()")
    @DeleteMapping("/{commentId}")
    public ResponseEntity<Void> deleteComment(
            @PathVariable String shortCode,
            @PathVariable Long commentId,
            @RequestHeader("Authorization") String authorizationHeader
    ) {
        commentService.deleteComment(shortCode, commentId, authorizationHeader);

        return ResponseEntity.noContent().build();
    }

    // 좋아요 토글
    @PostMapping("/{commentId}/like")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<Map<String, Object>> likeComment(
            @PathVariable String shortCode,
            @PathVariable Long commentId,
            @RequestHeader("Authorization") String authorizationHeader
    ) {

        boolean liked = commentLikeService.toggleLike(shortCode, commentId, authorizationHeader);
        long likeCount = stockCommentLikeRepository.countByStockComment_Id(commentId);

        Map<String, Object> response = new HashMap<>();
        response.put("liked", liked);
        response.put("likeCount", likeCount);

        return ResponseEntity.ok(response);
    }
}
