import { Card, CardContent } from "@/shared/libs/ui/card";

const BotProfileCard = ({ name, profitRate }: { name: string; profitRate: number }) => {
  return (
    <Card
      className={`cursor-pointer transition-colors hover:bg-muted/50`}
      // onClick={() => onSelectBot(bot.id)}
    >
      <CardContent className="p-4">
        <div className="flex items-center gap-3">
          <div className="flex size-10 items-center justify-center rounded-full bg-primary/10 text-xl">
            🤖
          </div>
          <div className="flex-1">
            <h3 className="truncate font-semibold">{name}</h3>
            <div
              className={`flex items-center text-sm font-medium ${
                profitRate >= 0 ? "text-red-500" : "text-blue-500"
              }`}
            >
              {profitRate >= 0 ? "+" : ""}
              {profitRate.toFixed(1)}%
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default BotProfileCard;
