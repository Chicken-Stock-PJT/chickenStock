import { DollarSign, TrendingUp } from "lucide-react";

const AiInfo = ({
  aiBotId,
  totalAsset,
  memberMoney,
  totalReturnRate,
  totalProfitLoss,
  todayReturnRate,
  todayProfitLoss,
}: {
  aiBotId: number;
  totalAsset: number;
  totalReturnRate: number;
  totalProfitLoss: number;
  memberMoney: number;
  todayReturnRate: number;
  todayProfitLoss: number;
}) => {
  const Bot = () => {
    switch (aiBotId) {
      case 1:
        return {
          name: "귀요미 AI",
          tactic: "엔벨로프(Envelope) 전략",
          description:
            "엔벨로프 전략은 이동평균선을 기준으로 상단과 하단 밴드를 설정하여 주가의 과열 및 침체 구간을 판단합니다. 주가가 하단 밴드를 터치하면 매수하고, 중앙선이나 상단선에 도달하면 순차적으로 매도합니다. 절반 매도와 전량 매도의 이분화된 구조로 수익 실현 전략을 명확히 합니다. 비교적 단순하지만 강건한 구조로, 추세 반전 구간에서 유용하게 작동합니다.",
        };
      case 2:
        return {
          name: "쿨한 AI",
          tactic: "단타 매매 전략 (거래량 & 눌림목 기반)",
          description:
            "이 전략은 단기적인 거래량 급증과 가격 눌림목 패턴을 포착해 빠르게 진입/이탈하는 구조입니다. 거래대금 상위 종목 중 유망 종목을 선별하고, 분할 매수·매도를 통해 리스크를 분산합니다. 눌림목 패턴, 기준선 접근, 손절·익절 조건을 복합적으로 활용하여 정밀한 진입/청산 시점을 도출합니다. 빠른 매매 회전율을 기반으로 한 적극적인 단타 전략입니다.",
        };
      case 3:
        return {
          name: "chill~AI",
          tactic: "볼린저 밴드 전략",
          description:
            "볼린저 밴드 전략은 가격이 통계적으로 평균에서 얼마나 벗어났는지를 측정하여 매매 타이밍을 포착합니다. 주가가 하단 밴드에 근접하거나 도달하면 매수를, 상단 밴드에 근접하면 매도를 유도합니다. %B 값이 낮을 때 추가 매수 기회를 탐색하고, 급격한 하락 시 손절 조건도 설정되어 있습니다. 안정성과 리스크 관리를 겸비한 전략으로, 변동성이 높은 시장에서도 효과적으로 작동합니다.",
        };
      // case 4:
      //   return {
      //     name: "미니 AI",
      //     description:
      //       "조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매조그매",
      //   };
      default:
        return {
          name: "귀요미 AI",
          description:
            "엔벨로프 전략은 이동평균선을 기준으로 상단과 하단 밴드를 설정하여 주가의 과열 및 침체 구간을 판단합니다. 주가가 하단 밴드를 터치하면 매수하고, 중앙선이나 상단선에 도달하면 순차적으로 매도합니다. 절반 매도와 전량 매도의 이분화된 구조로 수익 실현 전략을 명확히 합니다. 비교적 단순하지만 강건한 구조로, 추세 반전 구간에서 유용하게 작동합니다.",
        };
    }
  };
  return (
    <div className="space-y-6 rounded-xl border p-6 text-left shadow">
      <div className="grid grid-cols-2 gap-2 md:grid-cols-1">
        <div className="text-lg font-semibold">{Bot().name}</div>
        <div className="text-foreground">{Bot().tactic}: </div>
        <div className="text-sm text-foreground">{Bot().description}</div>
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
          <div className="mb-1 text-sm font-medium text-muted-foreground">총 수익</div>
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
              {totalProfitLoss > 0 ? "+" : ""}({totalProfitLoss.toLocaleString()}
              원)
            </span>
          </div>
        </div>
        <div>
          <div className="mb-1 text-sm font-medium text-muted-foreground">금일 수익</div>
          <div className="flex items-center">
            <TrendingUp className="mr-2 size-4 text-green-500" />
            <span
              className={`text-lg font-semibold ${
                todayReturnRate > 0
                  ? "text-chart-red"
                  : todayReturnRate < 0
                    ? "text-chart-blue"
                    : ""
              }`}
            >
              {todayReturnRate > 0 ? "+" : ""}
              {todayReturnRate.toFixed(2)}%
            </span>
            <span className="ml-2 text-muted-foreground">
              {todayProfitLoss > 0 ? "+" : ""}({todayProfitLoss.toLocaleString()}원)
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
