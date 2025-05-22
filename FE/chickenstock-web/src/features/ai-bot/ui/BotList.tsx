import { botList } from "@/features/ai-bot/model/bot-list";
import BotProfileCard from "./BotProfileCard";
import { Button } from "@/shared/libs/ui/button";
import { ChevronDown } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/shared/libs/ui/dropdown-menu";

const BotList = ({
  selectedBot,
  setSelectedBot,
}: {
  selectedBot: number;
  setSelectedBot: (bot: number) => void;
}) => {
  return (
    <div className="flex flex-col gap-4">
      <main className="hidden gap-4 sm:flex sm:flex-row lg:flex-col">
        {botList.map((bot) => (
          <BotProfileCard
            key={bot.id}
            {...bot}
            onClick={() => setSelectedBot(bot.id)}
            isSelected={selectedBot === bot.id}
          />
        ))}
      </main>
      <div className="sm:hidden">
        <DropdownMenu>
          <DropdownMenuTrigger className="w-full rounded-md bg-gray-100 p-0">
            <Button className="flex w-full items-center justify-between">
              <span className="mx-auto">{botList[selectedBot - 1].name}</span>
              <ChevronDown />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent className="w-full">
            {botList.map((bot) => (
              <DropdownMenuItem key={bot.id} onClick={() => setSelectedBot(bot.id)}>
                {bot.name}
              </DropdownMenuItem>
            ))}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
    </div>
  );
};

export default BotList;
