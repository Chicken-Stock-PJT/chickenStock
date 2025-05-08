import { useAuthStore } from "@/shared/store/auth";
import { DollarSign, TrendingUp } from "lucide-react";

interface User {
  email: string;
  nickname: string;
  joinDate: string;
  accountBalance: string;
  totalProfit: number;
  profitRate: string;
  ranking: number;
  totalUsers: number;
  rankingPercentile: number;
}

const UserInfo = () => {
  const { simpleProfile } = useAuthStore();
  const user: User = {
    email: "user@example.com",
    nickname: simpleProfile?.nickname ?? "",
    joinDate: "2023-01-15",
    accountBalance: simpleProfile?.memberMoney ?? "0", // 계좌잔고
    totalProfit: Number(simpleProfile?.memberMoney ?? 0) * Number(simpleProfile?.returnRate ?? 0), // 총 수익
    profitRate: simpleProfile?.returnRate ?? "0.00%",
    // todayProfit: 125000,
    // todayProfitRate: 1.2,
    ranking: 342,
    totalUsers: 10542,
    rankingPercentile: 3.2,
  };

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
              <div className="text-lg font-semibold">{user.nickname}</div>
            </div>
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">이메일</div>
              <div>{user.email}</div>
            </div>
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
                  {user.accountBalance.toLocaleString()}원
                </span>
              </div>
            </div>
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">총 수익률</div>
              <div className="flex items-center">
                <TrendingUp className="mr-2 size-4 text-green-500" />
                <span className="text-lg font-semibold text-green-500">+{user.profitRate}%</span>
                <span className="ml-2 text-muted-foreground">
                  (+{user.totalProfit.toLocaleString()}원)
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
