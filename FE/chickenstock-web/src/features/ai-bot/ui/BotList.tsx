import BotProfileCard from "./BotProfileCard";

const BotList = ({
  selectedBot,
  setSelectedBot,
}: {
  selectedBot: number;
  setSelectedBot: (bot: number) => void;
}) => {
  const mock = [
    {
      id: 1,
      name: "귀요미 AI",
      type: "밸류 투자형",
      description: "기업의 내재가치를 분석하여 저평가된 주식에 장기 투자하는 전략을 사용합니다.",
      profitRate: 24.8,
    },
    {
      id: 2,
      name: "쿨한 AI",
      type: "기술적 분석형",
      description:
        "차트 패턴과 기술적 지표를 분석하여 단기 매매 기회를 포착하는 전략을 사용합니다.",
      profitRate: 31.2,
    },
    {
      id: 3,
      name: "chill~AI",
      type: "배당 투자형",
      description: "안정적인 배당금을 지급하는 우량 기업에 투자하여 장기적인 수익을 추구합니다.",
      profitRate: 15.6,
    },
    {
      id: 4,
      name: "미니 AI",
      type: "모멘텀 투자형",
      description:
        "상승 추세에 있는 주식을 매수하고 하락 추세로 전환될 때 매도하는 추세 추종 전략을 사용합니다.",
      profitRate: 20.3,
    },
  ];
  return (
    <div className="flex flex-col gap-4">
      <main className="flex flex-row gap-4 lg:flex-col">
        {mock.map((bot) => (
          <BotProfileCard
            key={bot.id}
            {...bot}
            onClick={() => setSelectedBot(bot.id)}
            isSelected={selectedBot === bot.id}
          />
        ))}
      </main>
    </div>
  );
};

export default BotList;
