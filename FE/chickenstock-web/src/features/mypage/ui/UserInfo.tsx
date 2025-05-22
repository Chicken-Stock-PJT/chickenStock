// UserInfo.tsx
import { SimpleProfile } from "@/features/mypage/model/types";
import { DollarSign, TrendingUp } from "lucide-react";
import { InitializeMoneyDialog } from "./InitializeMoneyDialog";

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
          </div>
          <div className="space-y-4">
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">총 자산</div>
              <div className="flex items-center">
                <DollarSign className="mr-2 size-4 text-muted-foreground" />
                <span className="text-lg font-semibold">
                  {Number(simpleProfile.totalAsset).toLocaleString()}원
                </span>
              </div>
            </div>
            <div>
              <div className="mb-1 text-sm font-medium text-muted-foreground">총 수익률</div>
              <div className="flex items-center">
                <TrendingUp className="mr-2 size-4 text-green-500" />
                <span
                  className={`text-lg font-semibold ${
                    Number(simpleProfile?.returnRate) > 0
                      ? "text-chart-red"
                      : Number(simpleProfile?.returnRate) < 0
                        ? "text-chart-blue"
                        : ""
                  }`}
                >
                  {Number(simpleProfile?.returnRate) > 0 ? "+" : ""}
                  {Number(simpleProfile?.returnRate).toFixed(2)}%
                </span>
                <span className="ml-2 text-muted-foreground">
                  {Number(simpleProfile.totalAsset) > 100000000 ? "+" : ""}(
                  {(Number(simpleProfile.totalAsset) - 100000000).toLocaleString()}
                  원)
                </span>
              </div>
            </div>
          </div>
        </div>
        <div className="mt-6 flex justify-end">
          <InitializeMoneyDialog />
        </div>
      </main>
    </div>
  );
};

export default UserInfo;
