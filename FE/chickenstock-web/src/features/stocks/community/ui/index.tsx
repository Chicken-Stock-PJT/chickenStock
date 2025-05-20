import React, { useState, useEffect } from "react";
import { useParams, useLocation } from "react-router-dom";
import { useCommentsQuery } from "../model/queries";
import CommentItem from "./CommentItem";
import CommentForm from "./CommentForm";
import AlertModal from "./AlertModal";
import { Comment, CommunityPageProps } from "../model/types";

const CommunityPage: React.FC<CommunityPageProps> = ({ stockName: propStockName }) => {
  const location = useLocation();
  const state = location.state as { stockName?: string };
  const [stockName, setStockName] = useState<string>(() => {
    return state?.stockName ?? "종목";
  });
  const { stockCode } = useParams<{ stockCode: string }>();
  const [limit] = useState(10);
  const [cursor, setCursor] = useState<string | undefined>(undefined);
  const [allComments, setAllComments] = useState<Comment[]>([]);
  const [alertModal, setAlertModal] = useState({
    show: false,
    title: "",
    message: "",
  });

  // 댓글 목록 조회 쿼리 실행
  const { data, isLoading, isError, error } = useCommentsQuery({
    stockCode: stockCode ?? "",
    limit,
    cursor,
  });

  // 데이터가 로드되면 댓글 목록 누적
  useEffect(() => {
    if (data?.comments) {
      if (cursor) {
        // 추가 페이지인 경우 기존 댓글에 추가
        setAllComments((prev) => [...prev, ...data.comments]);
      } else {
        // 첫 페이지인 경우 댓글 목록 새로 설정
        setAllComments(data.comments);
      }
    }
  }, [data?.comments, cursor]);

  // 종목명 설정
  useEffect(() => {
    if (propStockName) {
      setStockName(propStockName);
    }
  }, [propStockName]);

  // 더 많은 댓글 로드
  const handleLoadMore = () => {
    if (data?.nextCursor) {
      setCursor(data.nextCursor);
    }
  };

  const closeAlert = () => {
    setAlertModal({
      ...alertModal,
      show: false,
    });
  };

  return (
    <div className="flex w-full flex-col rounded-lg bg-gray-50 p-4">
      <div className="mb-6 flex items-center justify-between">
        <h1 className="text-xl font-bold">{stockName} 커뮤니티</h1>
      </div>

      {/* 댓글 작성 폼 */}
      {stockCode && <CommentForm stockCode={stockCode} />}

      {isLoading && cursor === undefined && (
        <div className="flex justify-center py-4">
          <div className="size-6 animate-spin rounded-full border-y-2 border-yellow-500"></div>
        </div>
      )}

      {isError && (
        <div className="mb-4 rounded-lg bg-red-100 p-4 text-left text-red-700">
          댓글을 불러오는데 오류가 발생했습니다:{" "}
          {error instanceof Error ? error.message : "알 수 없는 오류"}
        </div>
      )}

      <div className="flex flex-col gap-4">
        {allComments.length > 0
          ? allComments.map((comment) => <CommentItem key={comment.id} comment={comment} />)
          : !isLoading && (
              <div className="rounded-lg bg-white p-6 text-left text-gray-500">
                아직 댓글이 없습니다. 첫 댓글을 작성해보세요!
              </div>
            )}
      </div>

      {isLoading && cursor !== undefined && (
        <div className="mt-4 flex justify-center py-4">
          <div className="size-6 animate-spin rounded-full border-y-2 border-yellow-500"></div>
        </div>
      )}

      {data?.nextCursor && !isLoading && (
        <button
          onClick={handleLoadMore}
          className="mt-4 px-4 py-2 rounded-lg bg-gray-100 text-gray-700 font-medium hover:bg-gray-200 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          더 보기
        </button>
      )}

      <AlertModal
        show={alertModal.show}
        title={alertModal.title}
        message={alertModal.message}
        onConfirm={closeAlert}
      />
    </div>
  );
};

export default CommunityPage;
