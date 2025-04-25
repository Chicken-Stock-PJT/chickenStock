import { useState, useRef } from "react";
import { Minus, Plus } from "lucide-react";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/widgets/ui/tabs";

const Trade = () => {
  const [isLimitOrder, setIsLimitOrder] = useState<boolean>(false); // true: 지정가, false: 시장가
  const [quantity, setQuantity] = useState<number>(1); // 수량
  const currentPrice = 150000; // 현재 가격 (예시로 100000으로 설정, 실제로는 API에서 받아와야 함)
  const lowPrice = 50000; // 최저 가격 (예시로 50000으로 설정, 실제로는 API에서 받아와야 함)
  const [price, setPrice] = useState<number>(currentPrice); // 가격 (지정가 주문 시 사용)
  const [tempPrice, setTempPrice] = useState<string>(""); // 임시 가격을 저장할 상태 추가
  const quantityInputRef = useRef<HTMLInputElement>(null); // 수량 input ref
  const priceInputRef = useRef<HTMLInputElement>(null); // 가격 input ref

  const handlePriceType = (toLimitOrder: boolean) => {
    if (toLimitOrder) {
      alert("지정가로 주문할 수 없는 주식입니다.");
      return;
    }

    setIsLimitOrder(toLimitOrder ? true : false);
    if (!toLimitOrder) {
      setPrice(currentPrice); // 시장가 주문 시 현재 가격으로 설정
    }
  };

  // 가격에 따라 tick size를 결정
  const getTickSize = (price: number): number => {
    if (price < 2000) return 1;
    if (price < 5000) return 5;
    if (price < 20000) return 10;
    if (price < 50000) return 50;
    if (price < 200000) return 100;
    if (price < 500000) return 500;
    return 1000;
  };

  // 수량 변경 핸들러
  const handleQuantityChange = (value: string) => {
    const numValue = Number(value);
    if (!isNaN(numValue) && numValue > 0) {
      const newValue = Math.ceil(numValue / 1);
      setQuantity(newValue);
    }
  };

  // 가격 입력 핸들러
  const handlePriceInput = (value: string) => {
    setTempPrice(value);
  };

  // 가격 변경 핸들러 (blur 시 호출)
  const handlePriceChange = () => {
    const numValue = Number(tempPrice);
    if (!isNaN(numValue) && numValue > 0) {
      const tickSize = getTickSize(numValue);
      const newPrice = Math.ceil(numValue / tickSize) * tickSize;
      setPrice(newPrice);
    }
    setTempPrice(""); // 임시 가격 초기화
  };

  // 수량 감소 핸들러
  const handleQuantityDecrease = () => {
    if (quantity > 0) {
      setQuantity(quantity - 1);
      quantityInputRef.current?.focus();
    }
  };

  // 수량 증가 핸들러
  const handleQuantityIncrease = () => {
    setQuantity(quantity + 1);
    quantityInputRef.current?.focus();
  };

  // 가격 감소 핸들러
  const handlePriceDecrease = () => {
    if (isLimitOrder && price > 0) {
      const tickSize = getTickSize(price);
      setPrice(price - tickSize);
      priceInputRef.current?.focus();
    }
  };

  // 가격 증가 핸들러
  const handlePriceIncrease = () => {
    if (isLimitOrder) {
      const tickSize = getTickSize(price);
      setPrice(price + tickSize);
      priceInputRef.current?.focus();
    }
  };

  return (
    <div className="w-full rounded-lg border border-gray-200 bg-white p-4 text-left">
      <Tabs defaultValue="buy" className="w-full">
        <div className="mb-4 flex items-center justify-between">
          <span className="font-bold text-gray-800">주문하기</span>
          <TabsList className="flex gap-1.5">
            <TabsTrigger
              value="buy"
              className="
                w-full 
                text-gray-500 
                data-[state=active]:font-semibold
                data-[state=active]:text-chart-red
              "
            >
              매수
            </TabsTrigger>
            <TabsTrigger
              value="sell"
              className="
                w-full 
                text-gray-500 
                data-[state=active]:font-semibold
                data-[state=active]:text-chart-blue
              "
            >
              매도
            </TabsTrigger>
          </TabsList>
        </div>

        <div className="mt-4 flex items-center gap-4">
          <button
            className={`w-full rounded-lg px-4 py-2 transition-colors ${
              !isLimitOrder
                ? "border-2 border-primary-400 bg-primary-50 text-gray-900"
                : "bg-gray-100 text-gray-600 hover:bg-gray-200"
            }`}
            onClick={() => handlePriceType(false)}
          >
            시장가
          </button>
          <button
            className={`w-full rounded-lg px-4 py-2 transition-colors ${
              isLimitOrder
                ? "border-2 border-primary-400 bg-primary-50 text-gray-900"
                : "bg-gray-100 text-gray-600 hover:bg-gray-200"
            }`}
            onClick={() => handlePriceType(true)}
          >
            지정가
          </button>
        </div>

        <div className="mt-4 flex flex-col gap-2">
          <label htmlFor="quantity" className="text-sm text-gray-500">
            수량
          </label>
          <div className="relative flex items-center">
            <button
              onClick={handleQuantityDecrease}
              className="absolute left-2 p-1 text-gray-500 hover:text-primary-500"
            >
              <Minus size={16} />
            </button>
            <input
              ref={quantityInputRef}
              type="number"
              id="quantity"
              value={quantity}
              onChange={(e) => handleQuantityChange(e.target.value)}
              onClick={(e) => e.currentTarget.select()}
              min="0"
              className="w-full rounded-lg border px-8 py-2 text-center focus:outline-none focus:ring-2 focus:ring-primary-500 [&::-webkit-inner-spin-button]:appearance-none [&::-webkit-outer-spin-button]:appearance-none"
            />
            <button
              onClick={handleQuantityIncrease}
              className="absolute right-2 p-1 text-gray-500 hover:text-primary-500"
            >
              <Plus size={16} />
            </button>
          </div>
        </div>

        <div className="mt-4 flex flex-col gap-2">
          <label htmlFor="price" className="text-sm text-gray-500">
            가격
          </label>
          <div className="relative flex items-center">
            <button
              onClick={handlePriceDecrease}
              className={`absolute left-2 p-1 text-gray-500 ${isLimitOrder ? "hover:text-primary-500" : "disabled:cursor-not-allowed disabled:opacity-50"}`}
              disabled={!isLimitOrder}
            >
              <Minus size={16} />
            </button>
            <input
              ref={priceInputRef}
              type="number"
              id="price"
              value={tempPrice || price}
              onChange={(e) => handlePriceInput(e.target.value)}
              onBlur={handlePriceChange}
              onClick={(e) => e.currentTarget.select()}
              readOnly={!isLimitOrder}
              min={lowPrice}
              className={`w-full rounded-lg border px-8 py-2 text-center focus:outline-none focus:ring-2 focus:ring-primary-500 [&::-webkit-inner-spin-button]:appearance-none [&::-webkit-outer-spin-button]:appearance-none
              ${!isLimitOrder ? "cursor-not-allowed bg-gray-100" : ""}`}
            />
            <button
              onClick={handlePriceIncrease}
              className={`absolute right-2 p-1 text-gray-500 ${isLimitOrder ? "hover:text-primary-500" : "disabled:cursor-not-allowed disabled:opacity-50"}`}
              disabled={!isLimitOrder}
            >
              <Plus size={16} />
            </button>
          </div>
        </div>

        <div className="mb-4 mt-8 flex justify-between gap-2 text-lg">
          <span>총 가격</span>
          <span>{(price * quantity).toLocaleString()}원</span>
        </div>
        <TabsContent value="buy">
          <button
            className="w-full rounded-lg bg-primary-300 py-3 
        text-gray-900 drop-shadow-[0_0px_1px_rgba(0,0,0,0.1)]
        transition-all
        duration-200 hover:bg-primary-400 active:scale-[0.98]
        active:shadow-inner"
            onClick={() => {
              // 매수 신청 로직
              console.log("매수 신청", {
                quantity,
                price,
                isLimitOrder,
              });
            }}
          >
            매수 신청하기
          </button>
        </TabsContent>
        <TabsContent value="sell">
          <button
            className="w-full rounded-lg bg-primary-300 py-3 
        text-gray-900 drop-shadow-[0_0px_1px_rgba(0,0,0,0.1)]
        transition-all
        duration-200 hover:bg-primary-400 active:scale-[0.98]
        active:shadow-inner"
            onClick={() => {
              // 매도 신청 로직
              console.log("매도 신청", {
                quantity,
                price,
                isLimitOrder,
              });
            }}
          >
            매도 신청하기
          </button>
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default Trade;
