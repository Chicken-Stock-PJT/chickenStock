import { DollarSign, TrendingUp, Trophy } from "lucide-react";

const AiInfo = ({
  aiName,
  totalAsset,
  memberMoney,
  totalReturnRate,
}: {
  aiName: string;
  totalAsset: number;
  totalReturnRate: number;
  memberMoney: number;
}) => {
  return (
    <div className="space-y-6 rounded-xl border p-6 text-left shadow">
      <div className="grid grid-cols-2 gap-6 md:grid-cols-1">
        <div className="text-lg font-semibold">{aiName}</div>
        <div className="flex items-start text-sm">
          <Trophy className="mr-2 size-4" />
          <span>랭킹 1위</span>
        </div>
      </div>
      <div className="space-y-4">
        <div>
          <div className="mb-1 text-sm font-medium text-muted-foreground">총 자산</div>
          <div className="flex items-center">
            <DollarSign className="mr-2 size-4 text-muted-foreground" />
            <span className="text-lg font-semibold">{totalAsset.toLocaleString()}원</span>
          </div>
        </div>
        <div>
          <div className="mb-1 text-sm font-medium text-muted-foreground">총 수익률</div>
          <div className="flex items-center">
            <TrendingUp className="mr-2 size-4 text-green-500" />
            <span
              className={`text-lg font-semibold ${
                totalReturnRate > 0
                  ? "text-chart-red"
                  : totalReturnRate < 0
                    ? "text-chart-blue"
                    : ""
              }`}
            >
              {totalReturnRate > 0 ? "+" : ""}
              {totalReturnRate.toFixed(2)}%
            </span>
            <span className="ml-2 text-muted-foreground">
              {totalAsset > 100000000 ? "+" : ""}({(totalAsset - 100000000).toLocaleString()}
              원)
            </span>
          </div>
        </div>
      </div>
      <div>
        <div className="flex h-4 w-full rounded-full bg-gray-200">
          <div
            className="h-4 rounded-l-full bg-primary-400"
            style={{ width: `${(memberMoney / totalAsset) * 100}%` }}
          ></div>
          <div
            className="h-4 rounded-r-full bg-[#00C49F]"
            style={{ width: `${((totalAsset - memberMoney) / totalAsset) * 100}%` }}
          ></div>
        </div>
        <div className="space-y-1">
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-primary-400"></div>
              <div>현금</div>
            </div>
            <div className="flex items-center gap-1.5 font-semibold">
              <span>{memberMoney.toLocaleString()}원</span>
              <span className="text-sm text-muted-foreground ">
                ({((memberMoney / totalAsset) * 100).toFixed(2)}%)
              </span>
            </div>
          </div>
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-[#00C49F]"></div>
              <div>주식</div>
            </div>
            <div className="flex items-center gap-1.5 font-semibold">
              <span>{(totalAsset - memberMoney).toLocaleString()}원</span>
              <span className="text-sm text-muted-foreground ">
                ({(((totalAsset - memberMoney) / totalAsset) * 100).toFixed(2)}%)
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AiInfo;
