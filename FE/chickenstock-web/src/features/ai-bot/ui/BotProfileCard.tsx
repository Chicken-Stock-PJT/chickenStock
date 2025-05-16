import { Card, CardContent } from "@/shared/libs/ui/card";

const BotProfileCard = ({
  name,
  isSelected,
  onClick,
}: {
  name: string;
  profitRate: number;
  isSelected: boolean;
  onClick: () => void;
}) => {
  return (
    <Card
      className={`cursor-pointer transition-colors hover:bg-gray-100 ${
        isSelected ? "bg-gray-100" : ""
      }`}
      onClick={onClick}
    >
      <CardContent className="p-4">
        <div className="flex items-center gap-3">
          <div className="flex size-10 items-center justify-center rounded-full bg-primary/10 text-xl">
            🤖
          </div>
          <div className="flex-1">
            <h3 className="truncate font-semibold">{name}</h3>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default BotProfileCard;
