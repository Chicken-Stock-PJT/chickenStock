import { useState, useMemo, useEffect } from "react";
import { useStocksQuery } from "../hooks/useStocksQuery";
import {
  Dialog,
  DialogClose,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/shared/libs/ui/dialog";
import { Input } from "@/shared/libs/ui/input";
import { Search } from "lucide-react";
import { useNavigate } from "react-router-dom";

interface SearchModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export default function SearchModal({ open, onOpenChange }: SearchModalProps) {
  const [searchTerm, setSearchTerm] = useState("");
  const navigate = useNavigate();

  const { data: allStocks = [], isLoading, isFetching } = useStocksQuery();

  const filteredResults = useMemo(() => {
    if (!searchTerm.trim()) return allStocks;

    return allStocks.filter(
      (stock) =>
        stock.shortName.toLowerCase().includes(searchTerm.toLowerCase()) ||
        stock.shortCode.includes(searchTerm),
    );
  }, [searchTerm, allStocks]);

  // 모달 열릴 때 검색어 초기화
  useEffect(() => {
    if (open) {
      setSearchTerm("");
      setTimeout(() => {
        const searchInput = document.getElementById("search-input");
        if (searchInput) {
          searchInput.focus();
        }
      }, 100);
    }
  }, [open]);

  const toStockPage = (stockCode: string) => {
    void navigate(`stocks/${stockCode}`);
    onOpenChange(false); // 페이지 이동 시 모달 닫기
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="flex max-h-[80vh] flex-col overflow-hidden sm:max-w-[600px]">
        <DialogHeader>
          <DialogTitle>종목 검색</DialogTitle>
        </DialogHeader>
        <div className="relative mb-4">
          <Search className="absolute left-3 top-3 size-4 text-muted-foreground" />
          <Input
            id="search-input"
            className="pl-10"
            placeholder="종목명 또는 코드 검색"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>
        <div className="-mx-6 flex-1 justify-between overflow-y-auto px-6">
          {isLoading || isFetching ? (
            <div className="py-8 text-center text-muted-foreground">검색 중...</div>
          ) : filteredResults.length > 0 ? (
            <div>
              {filteredResults.map((stock) => (
                <DialogClose
                  key={stock.shortCode}
                  className="flex w-full cursor-pointer items-center justify-between bg-white py-3 hover:bg-gray-100"
                  onClick={() => toStockPage(stock.shortCode)}
                >
                  <div className="flex items-center gap-4 text-left">
                    <img
                      className="size-8 rounded-lg"
                      src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stock.shortCode}.png`}
                      alt=""
                    />
                    <div>
                      <div className="font-medium">{stock.shortName}</div>
                      <div className="text-xs text-muted-foreground">{stock.shortCode}</div>
                    </div>
                  </div>
                  <div className="flex items-center gap-4">
                    <div className="text-right">
                      <div className="font-medium">{stock.market}</div>
                      <div className="text-xs text-muted-foreground">{stock.stockType}</div>
                    </div>
                  </div>
                </DialogClose>
              ))}
            </div>
          ) : (
            <div className="py-8 text-center text-muted-foreground">
              {searchTerm.trim() === "" ? "검색어를 입력하세요" : "검색 결과가 없습니다."}
            </div>
          )}
        </div>
      </DialogContent>
    </Dialog>
  );
}
