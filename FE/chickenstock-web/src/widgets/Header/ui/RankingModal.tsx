import { User } from "lucide-react";
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

  // 마지막 갱신 시간 계산
  const getLastUpdateTime = () => {
    const now = new Date();
    const lastHour = new Date(now);
    lastHour.setMinutes(0, 0, 0);

    // 다음 갱신 시간까지 남은 분 계산
    const minutesLeft = 60 - now.getMinutes();

    return {
      lastUpdate: lastHour.toLocaleTimeString("ko-KR", { hour: "2-digit", minute: "2-digit" }),
      nextUpdate: minutesLeft,
    };
  };

  const { lastUpdate, nextUpdate } = getLastUpdateTime();

  return (
    <Modal open={open} onOpenChange={onOpenChange}>
      <ModalContent className="max-h-[90vh] max-w-md overflow-y-auto">
        <ModalHeader>
          <ModalTitle className="flex items-center gap-2">
            <div className="size-2 text-yellow-500" />
            자산 랭킹
          </ModalTitle>
        </ModalHeader>

        <div className="space-y-4">
          {isLoading ? (
            <div className="p-4 text-center">로딩 중...</div>
          ) : error ? (
            <div className="p-4 text-center text-red-500">
              오류가 발생했습니다. 다시 시도해주세요.
            </div>
          ) : (
            <>
              {/* 랭킹 갱신 정보 */}
              <div className="px-4 pb-2 pt-4 text-sm text-gray-500">
                <div className="flex items-center justify-between">
                  <span>마지막 갱신: {lastUpdate}</span>
                  <span>다음 갱신까지: {nextUpdate}분</span>
                </div>
              </div>

              {/* 상위 랭킹 */}
              <div className="px-4">
                {/* <h3 className="text-lg font-semibold">TOP {data?.topRankings.length ?? 0}</h3> */}
              </div>

              {/* 스크롤 가능한 랭킹 목록 */}
              <div className="max-h-96 overflow-y-auto px-4">
                <div className="space-y-2">
                  {data?.topRankings.map((ranking, index) => {
                    const isMyRanking =
                      isLogin && data.myRank && ranking.nickname === data.myRank.nickname;

                    return (
                      <div
                        key={index}
                        className={`flex items-center justify-between rounded-lg p-3 ${
                          isMyRanking
                            ? "border-2 border-orange-300 bg-orange-100"
                            : ranking.rank <= 3
                              ? "bg-yellow-50"
                              : "bg-gray-50"
                        }`}
                      >
                        <div className="flex items-center gap-3">
                          <div
                            className={`flex size-8 items-center justify-center rounded-full ${
                              ranking.rank === 1
                                ? "bg-yellow-500 text-white"
                                : ranking.rank === 2
                                  ? "bg-gray-400 text-white"
                                  : ranking.rank === 3
                                    ? "bg-orange-600 text-white"
                                    : "bg-gray-300 text-gray-700"
                            }`}
                          >
                            {ranking.rank}
                          </div>
                          <div className="flex items-center gap-2">
                            <span className="font-medium">{ranking.nickname}</span>
                            {isMyRanking && <User className="size-4 text-orange-600" />}
                          </div>
                        </div>
                        <span className="font-semibold">
                          ₩{ranking.totalAsset.toLocaleString()}
                        </span>
                      </div>
                    );
                  })}
                </div>
              </div>

              {/* 내 랭킹 (하단 고정) */}
              {isLogin && data?.myRank && (
                <div className="border-t px-4 pt-4">
                  <h3 className="mb-2 text-lg font-semibold">내 순위</h3>
                  <div className="flex items-center justify-between rounded-lg border-2 border-orange-300 bg-orange-100 p-3">
                    <div className="flex items-center gap-3">
                      <div className="flex size-8 items-center justify-center rounded-full bg-gray-400 text-white">
                        {data.myRank.rank}
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-medium">{data.myRank.nickname}</span>
                        <User className="size-4 text-orange-600" />
                      </div>
                    </div>
                    <span className="font-semibold">
                      ₩{data.myRank.totalAsset.toLocaleString()}
                    </span>
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
