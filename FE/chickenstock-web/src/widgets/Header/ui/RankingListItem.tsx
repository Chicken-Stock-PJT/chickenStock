// ui/RankingListItem.tsx
import React from "react";
import { User, Bot } from "lucide-react";
import { TotalAssetRanking, ReturnRateRanking, RankingType } from "../model/types";
import { useNavigate } from "react-router-dom";

interface RankingListItemProps {
  ranking: TotalAssetRanking | ReturnRateRanking;
  isMyRanking: boolean | undefined;
  rankingType: RankingType;
}

// AI 봇인지 확인하는 함수
const isAIBot = (nickname: string) => {
  const aiNames = ["쿨한 AI", "귀요미 AI", "chill~AI", "미니 AI"];
  return aiNames.includes(nickname);
};

const RankingListItem: React.FC<RankingListItemProps> = ({ ranking, isMyRanking, rankingType }) => {
  const navigate = useNavigate();
  const isBot = isAIBot(ranking.nickname);

  return (
    <div
      className={`flex items-center justify-between rounded-xl p-3.5 shadow-sm transition-all hover:shadow-md ${
        isMyRanking
          ? "border-2 border-orange-300 bg-gradient-to-r from-orange-50 to-orange-100"
          : isBot
            ? "border-2 border-blue-300 bg-gradient-to-r from-blue-50 to-blue-100"
            : ranking.rank <= 3
              ? "bg-gradient-to-r from-yellow-50 to-yellow-100 shadow-yellow-100"
              : "bg-white"
      }`}
      onClick={() => {
        void navigate(`/dashboard/${ranking.memberId}`, {
          state: { name: ranking.nickname },
        });
      }}
    >
      <div className="flex items-center gap-3">
        <div
          className={`flex size-9 items-center justify-center rounded-full shadow-sm ${
            ranking.rank === 1
              ? "bg-gradient-to-br from-yellow-400 to-yellow-600 text-white"
              : ranking.rank === 2
                ? "bg-gradient-to-br from-gray-300 to-gray-500 text-white"
                : ranking.rank === 3
                  ? "bg-gradient-to-br from-orange-400 to-orange-700 text-white"
                  : "bg-gradient-to-br from-gray-200 to-gray-300 text-gray-700"
          }`}
        >
          <span className="font-bold">{ranking.rank}</span>
        </div>
        <div className="flex items-center gap-2">
          <span className="font-medium text-gray-800">{ranking.nickname}</span>
          {isMyRanking && (
            <div className="rounded-full bg-orange-100 p-0.5">
              <User className="size-3.5 text-orange-600" />
            </div>
          )}
          {isBot && (
            <div className="rounded-full bg-blue-100 p-0.5">
              <Bot className="size-3.5 text-blue-600" />
            </div>
          )}
        </div>
      </div>
      <div className="flex items-center gap-2">
        {rankingType === "totalAsset" ? (
          <span className="font-semibold text-gray-900">
            ₩{(ranking as TotalAssetRanking).totalAsset.toLocaleString()}
          </span>
        ) : (
          <span
            className={`font-semibold ${(ranking as ReturnRateRanking).returnRate >= 0 ? "text-red-600" : "text-blue-600"}`}
          >
            {(ranking as ReturnRateRanking).returnRate.toFixed(2)}%
          </span>
        )}
        {/* {isBot && (
          <span className="rounded bg-blue-100 px-1.5 py-0.5 text-xs font-medium text-blue-700">
            AI
          </span>
        )} */}
      </div>
    </div>
  );
};

export default RankingListItem;
