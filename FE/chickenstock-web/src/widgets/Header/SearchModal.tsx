import { useState, useEffect } from "react";
import {
  Dialog,
  DialogClose,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/shared/libs/ui/dialog";
import { Input } from "@/shared/libs/ui/input";
import { Search, ArrowUpIcon, ArrowDownIcon } from "lucide-react";
import { useNavigate } from "react-router-dom";

// 모의 검색 결과 데이터
const mockSearchResults = [
  {
    code: "005930",
    name: "삼성전자",
    price: 72800,
    change: 1300,
    changePercent: 1.82,
  },
  {
    code: "000660",
    name: "SK하이닉스",
    price: 168500,
    change: 3500,
    changePercent: 2.12,
  },
  {
    code: "035720",
    name: "카카오",
    price: 56700,
    change: -1200,
    changePercent: -2.07,
  },
  {
    code: "035420",
    name: "NAVER",
    price: 216500,
    change: 2500,
    changePercent: 1.17,
  },
  {
    code: "051910",
    name: "LG화학",
    price: 498000,
    change: -7000,
    changePercent: -1.39,
  },
  {
    code: "373220",
    name: "LG에너지솔루션",
    price: 435000,
    change: 5000,
    changePercent: 1.16,
  },
  {
    code: "207940",
    name: "삼성바이오로직스",
    price: 789000,
    change: -11000,
    changePercent: -1.37,
  },
  {
    code: "006400",
    name: "삼성SDI",
    price: 456000,
    change: 8000,
    changePercent: 1.79,
  },
  {
    code: "068270",
    name: "셀트리온",
    price: 178500,
    change: -3500,
    changePercent: -1.92,
  },
  {
    code: "003550",
    name: "LG",
    price: 87600,
    change: 1200,
    changePercent: 1.39,
  },
];

interface SearchModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export default function SearchModal({ open, onOpenChange }: SearchModalProps) {
  const [searchTerm, setSearchTerm] = useState("");
  const [results, setResults] = useState(mockSearchResults);
  const navigate = useNavigate();

  // 검색어 변경 시 결과 필터링
  useEffect(() => {
    if (searchTerm.trim() === "") {
      setResults(mockSearchResults);
    } else {
      const filtered = mockSearchResults.filter(
        (stock) =>
          stock.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
          stock.code.includes(searchTerm),
      );
      setResults(filtered);
    }
  }, [searchTerm]);

  // 모달이 열릴 때 검색창에 포커스
  useEffect(() => {
    setSearchTerm("");
    if (open) {
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
          {results.length > 0 ? (
            <div>
              {results.map((stock) => (
                <DialogClose
                  key={stock.code}
                  className="flex w-full items-center justify-between bg-white py-3 hover:bg-gray-100"
                  onClick={() => toStockPage(stock.code)}
                >
                  <div className="flex items-center gap-4 text-left">
                    <img
                      className="size-8 rounded-lg"
                      src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stock.code}.png`}
                      alt=""
                    />
                    <div>
                      <div className="font-medium">{stock.name}</div>
                      <div className="text-xs text-muted-foreground">{stock.code}</div>
                    </div>
                  </div>
                  <div className="flex items-center gap-4">
                    <div className="text-right">
                      <div className="font-medium">{stock.price.toLocaleString()}원</div>
                      <div
                        className={`flex items-center justify-end text-xs ${
                          stock.change > 0
                            ? "text-red-500"
                            : stock.change < 0
                              ? "text-blue-500"
                              : "text-gray-500"
                        }`}
                      >
                        {stock.change > 0 && <ArrowUpIcon className="mr-1 size-3" />}
                        {stock.change < 0 && <ArrowDownIcon className="mr-1 size-3" />}
                        {stock.change > 0 ? "+" : ""}
                        {stock.changePercent.toFixed(2)}%
                      </div>
                    </div>
                  </div>
                </DialogClose>
              ))}
            </div>
          ) : (
            <div className="py-8 text-center text-muted-foreground">검색 결과가 없습니다.</div>
          )}
        </div>
      </DialogContent>
    </Dialog>
  );
}
