import { SimpleProfile } from "@/shared/store/types";
import { DollarSign, TrendingUp } from "lucide-react";

const UserInfo = ({ simpleProfile }: { simpleProfile: SimpleProfile }) => {
  return (
    <div className="space-y-6 rounded-xl border p-6 text-left shadow">
      <header>
        <div className="text-xl font-semibold">기본정보</div>
        <div className="mt-1.5 text-sm text-gray-400">회원 기본 정보를 확인합니다.</div>
      </header>
      <main>
        <div className="grid grid-cols-1 gap-6 md:grid-cols-2">
          <div className="space-y-4">
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">닉네임</div>
              <div className="text-lg font-semibold">{simpleProfile.nickname}</div>
            </div>
            {/* <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">이메일</div>
              <div>{user.email}</div>
            </div> */}
            {/* 가입일 */}
            {/* <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">가입일</div>
              <div className="flex items-center">
                <Calendar className="mr-2 size-4 text-muted-foreground" />
                {new Date(user.joinDate).toLocaleDateString("ko-KR", {
                  year: "numeric",
                  month: "long",
                  day: "numeric",
                })}
              </div>
            </div> */}
          </div>
          <div className="space-y-4">
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">계좌 자산</div>
              <div className="flex items-center">
                <DollarSign className="mr-2 size-4 text-muted-foreground" />
                <span className="text-lg font-semibold">
                  {simpleProfile.memberMoney.toLocaleString()}원
                </span>
              </div>
            </div>
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">총 수익률</div>
              <div className="flex items-center">
                <TrendingUp className="mr-2 size-4 text-green-500" />
                <span className="text-lg font-semibold text-green-500">
                  +{Number(simpleProfile.returnRate).toFixed(2)}%
                </span>
                <span className="ml-2 text-muted-foreground">
                  (+{simpleProfile.memberMoney.toLocaleString()}원)
                </span>
              </div>
            </div>
            {/* 랭킹 -  추후 구현 예정 */}
            {/* <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">투자자 랭킹</div>
              <div className="flex items-center">
                <Trophy className="mr-2 size-4 text-yellow-500" />
                <span className="text-lg font-semibold">
                  {user.ranking}위 / {user.totalUsers}명
                </span>
                <span className="ml-2 text-muted-foreground">(상위 {user.rankingPercentile}%)</span>
              </div>
            </div> */}
          </div>
        </div>
      </main>
    </div>
  );
};

export default UserInfo;
