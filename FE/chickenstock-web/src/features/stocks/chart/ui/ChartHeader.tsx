import { ChartHeaderProps } from "../model/types";

const ChartHeader = ({
  stockName,
  stockCode,
  currentPrice,
  priceChange,
  changeRate,
}: ChartHeaderProps) => {
  const formattedPrice = (price: string) => Number(price.slice(1)).toLocaleString();

  const isPriceUp = priceChange.startsWith("+");

  return (
    <div className="flex items-center gap-4">
      <img
        className="w-[78px] rounded-2xl"
        src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stockCode}.png`}
        alt=""
      />
      <div className="flex flex-col gap-2">
        <div className="flex items-end gap-2 text-left">
          <h1 className="text-xl font-bold text-gray-800">{stockName}</h1>
          <p className="text-gray-600">{stockCode}</p>
        </div>
        <div className="flex gap-2 text-left text-chart-blue">
          <p className="text-3xl font-semibold leading-none">{formattedPrice(currentPrice)}</p>
          <div className="flex items-end items-center gap-2">
            <span className="text-lg leading-none">
              {isPriceUp ? "▲" : "▼"} {formattedPrice(priceChange)}
            </span>
            <span className="leading-none text-opacity-25">({changeRate}%)</span>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ChartHeader;
