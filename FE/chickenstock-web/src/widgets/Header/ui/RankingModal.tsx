import { User, Trophy, Clock, RefreshCw } from "lucide-react";
import { useAuthStore } from "@/shared/store/auth";
import { useRankingQuery } from "../model/queries";
import { Modal, ModalContent, ModalHeader, ModalTitle } from "@/shared/libs/ui/modal";

interface RankingModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

const RankingModal = ({ open, onOpenChange }: RankingModalProps) => {
  const isLogin = useAuthStore((state) => state.isLoggedIn);
  const { data, isLoading, error } = useRankingQuery();

  // 마지막 갱신 시간 계산 (5분 간격)
  const getLastUpdateTime = () => {
    const now = new Date();
    // 현재 시간을 분 단위로 계산 (0부터 59)
    const currentMinutes = now.getMinutes();
    // 5분 간격으로 나누기 (0, 5, 10, 15 ... 55)
    const lastUpdateMinute = Math.floor(currentMinutes / 5) * 5;

    // 마지막 갱신 시간 설정
    const lastUpdateTime = new Date(now);
    lastUpdateTime.setMinutes(lastUpdateMinute, 0, 0);

    // 다음 갱신까지 남은 시간 계산 (분 단위)
    const nextUpdateMinute = (Math.floor(currentMinutes / 5) + 1) * 5;
    const minutesLeft = nextUpdateMinute - currentMinutes;

    return {
      lastUpdate: lastUpdateTime.toLocaleTimeString("ko-KR", {
        hour: "2-digit",
        minute: "2-digit",
      }),
      nextUpdate: minutesLeft,
    };
  };

  const { lastUpdate, nextUpdate } = getLastUpdateTime();

  return (
    <Modal open={open} onOpenChange={onOpenChange}>
      <ModalContent className="max-h-[90vh] max-w-md overflow-y-auto rounded-xl bg-gradient-to-b from-white to-gray-50 shadow-lg">
        <ModalHeader className="border-b border-gray-100 pb-3">
          <ModalTitle className="flex items-center gap-2 text-xl font-bold text-gray-800">
            <Trophy className="size-5 text-yellow-500" />
            자산 랭킹
          </ModalTitle>
        </ModalHeader>

        <div className="space-y-5">
          {isLoading ? (
            <div className="flex h-40 items-center justify-center">
              <div className="flex flex-col items-center gap-2">
                <RefreshCw className="size-8 animate-spin text-orange-500" />
                <p className="text-sm font-medium text-gray-600">랭킹 정보를 불러오는 중...</p>
              </div>
            </div>
          ) : error ? (
            <div className="flex h-40 items-center justify-center p-4 text-center">
              <div className="rounded-lg bg-red-50 p-4 text-red-600">
                <p className="font-medium">오류가 발생했습니다.</p>
                <p className="text-sm">잠시 후 다시 시도해주세요.</p>
              </div>
            </div>
          ) : (
            <>
              {/* 랭킹 갱신 정보 */}
              <div className="mx-4 rounded-lg bg-blue-50 p-3 text-sm">
                <div className="flex items-center justify-between text-blue-700">
                  <div className="flex items-center gap-1.5">
                    <Clock className="size-4" />
                    <span>마지막 갱신: {lastUpdate}</span>
                  </div>
                  <div className="flex items-center gap-1.5">
                    <RefreshCw className="size-4" />
                    <span>다음 갱신: {nextUpdate}분 후</span>
                  </div>
                </div>
              </div>

              {/* 상위 랭킹 */}
              <div className="px-4">
                <h3 className="mb-3 text-lg font-bold text-gray-700">
                  <span className="mr-2 text-yellow-500">TOP</span>
                  <span>100</span>
                </h3>
              </div>

              {/* 스크롤 가능한 랭킹 목록 */}
              <div className="max-h-96 overflow-y-auto px-4">
                <div className="space-y-3">
                  {data?.topRankings.map((ranking, index) => {
                    const isMyRanking =
                      isLogin && data.myRank && ranking.nickname === data.myRank.nickname;

                    return (
                      <div
                        key={index}
                        className={`flex items-center justify-between rounded-xl p-3.5 shadow-sm transition-all hover:shadow-md ${
                          isMyRanking
                            ? "border-2 border-orange-300 bg-gradient-to-r from-orange-50 to-orange-100"
                            : ranking.rank <= 3
                              ? "bg-gradient-to-r from-yellow-50 to-yellow-100 shadow-yellow-100"
                              : "bg-white"
                        }`}
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
                          </div>
                        </div>
                        <span className="font-semibold text-gray-900">
                          ₩{ranking.totalAsset.toLocaleString()}
                        </span>
                      </div>
                    );
                  })}
                </div>
              </div>

              {/* 내 랭킹 (하단 고정) */}
              {isLogin && data?.myRank && (
                <div className="border-t border-gray-100 px-4 pt-4">
                  <h3 className="mb-3 flex items-center gap-2 text-lg font-bold text-gray-700">
                    <User className="size-5 text-orange-500" />내 순위
                  </h3>
                  <div className="flex items-center justify-between rounded-xl border-2 border-orange-300 bg-gradient-to-r from-orange-50 to-orange-100 p-3.5 shadow-sm">
                    <div className="flex items-center gap-3">
                      <div className="flex size-9 items-center justify-center rounded-full bg-gradient-to-br from-orange-400 to-orange-600 font-bold text-white shadow-sm">
                        {data.myRank.rank}
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-medium text-gray-800">{data.myRank.nickname}</span>
                        <div className="rounded-full bg-orange-200 p-0.5">
                          <User className="size-3.5 text-orange-600" />
                        </div>
                      </div>
                    </div>
                    <div className="flex flex-col items-end">
                      <span className="text-xs text-gray-500">총 자산</span>
                      <span className="font-bold text-gray-900">
                        ₩{data.myRank.totalAsset.toLocaleString()}
                      </span>
                    </div>
                  </div>
                </div>
              )}
            </>
          )}
        </div>
      </ModalContent>
    </Modal>
  );
};

export default RankingModal;
