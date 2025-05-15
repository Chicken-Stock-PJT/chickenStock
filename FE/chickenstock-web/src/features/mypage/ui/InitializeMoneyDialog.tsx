import { useState } from "react";
import { RefreshCw } from "lucide-react";
import { Button } from "@/shared/libs/ui/button";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "@/shared/libs/ui/alert-dialog";
import { useToast } from "@/shared/libs/hooks/use-toast";
import { initializeMoney } from "../api";

export const InitializeMoneyDialog = () => {
  const [isLoading, setIsLoading] = useState(false);
  const [isOpen, setIsOpen] = useState(false);
  const { toast } = useToast();

  const handleInitializeMoney = async () => {
    setIsLoading(true);
    try {
      const response = await initializeMoney();
      toast({
        title: "초기화 완료",
        description: response.message,
      });
      setIsOpen(false);
      // 페이지 새로고침 또는 상태 업데이트
      window.location.reload();
    } catch (error) {
      toast({
        title: "초기화 실패",
        description: "자본금 갱신 기회가 없습니다. 매주 월요일마다 기회가 갱신됩니다.",
        variant: "destructive",
      });
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <AlertDialog open={isOpen} onOpenChange={setIsOpen}>
      <AlertDialogTrigger asChild>
        <Button variant="outline" className="flex items-center gap-2">
          <RefreshCw className="size-4" />
          자본금 초기화
        </Button>
      </AlertDialogTrigger>
      <AlertDialogContent>
        <AlertDialogHeader>
          <AlertDialogTitle>자본금을 초기화하시겠습니까?</AlertDialogTitle>
          <AlertDialogDescription>
            <div className="space-y-2">
              <p>자본금을 1억원으로 초기화합니다.</p>
              <p className="font-semibold text-yellow-600">
                ⚠️ 자본금 초기화는 1주일에 1번만 가능합니다.
              </p>
              <p className="text-sm">매주 월요일마다 초기화 기회가 갱신됩니다.</p>
              <p className="text-sm text-muted-foreground">이 작업은 되돌릴 수 없습니다.</p>
            </div>
          </AlertDialogDescription>
        </AlertDialogHeader>
        <AlertDialogFooter>
          <AlertDialogCancel>취소</AlertDialogCancel>
          <AlertDialogAction onClick={() => void handleInitializeMoney()} disabled={isLoading}>
            {isLoading ? "초기화 중..." : "초기화"}
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
};
