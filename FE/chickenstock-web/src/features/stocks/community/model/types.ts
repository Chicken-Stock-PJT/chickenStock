// 댓글 아이템 타입 정의
export interface Comment {
  id: number;
  content: string;
  nickname: string;
  createdAt: string;
  updatedAt: string;
  children: Comment[];
  likeCount: number;
  likedByMe: boolean;
  deleted: boolean;
}

// 댓글 목록 조회 API 응답 타입
export interface CommentsResponse {
  comments: Comment[];
  nextCursor: string | null;
}

// 댓글 목록 조회 API 매개변수 타입
export interface CommentsParams {
  stockCode: string;
  limit?: number;
  cursor?: string;
}

// 커뮤니티 페이지 Props 타입
export interface CommunityPageProps {
  stockName?: string;
}
