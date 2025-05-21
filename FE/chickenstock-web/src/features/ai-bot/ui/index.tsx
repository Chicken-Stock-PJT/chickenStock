import BotList from "./BotList";
import AiBotPortfolio from "./AiBotPortfolio";
import { useState } from "react";

const AiBotLayout = () => {
  const [selectedBot, setSelectedBot] = useState<number>(1);

  return (
    <div className="mb-10 p-4 text-left">
      <header className="my-10 flex flex-col gap-2">
        <h2 className="text-2xl font-bold">AI 트레이딩 봇</h2>
        <p className="text-sm text-gray-500">
          ChickenStock의 AI 트레이딩 봇들의 성과와 전략을 확인하고 자신의 투자 성과와 비교해보세요.
        </p>
      </header>
      <main className="flex flex-col gap-8 lg:flex-row">
        <aside className="lg:w-1/6">
          <BotList selectedBot={selectedBot} setSelectedBot={setSelectedBot} />
        </aside>
        <article className="flex flex-col gap-8">
          <AiBotPortfolio selectedBot={selectedBot} />
        </article>
      </main>
    </div>
  );
};

export default AiBotLayout;
