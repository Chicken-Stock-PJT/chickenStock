const MyAsset = ({ totalAsset, memberMoney }: { totalAsset: number; memberMoney: number }) => {
  const assetRatio = {
    cash: Number(memberMoney ?? 0),
    stock: Number(totalAsset) - Number(memberMoney ?? 0), // 아직 데이터 X
  };

  return (
    <div className="space-y-6 rounded-xl border p-6 text-left shadow">
      <div className="space-y-4">
        <div className="pb-2 text-xl font-semibold">자산 비율</div>
        <div className="flex h-4 w-full rounded-full bg-gray-200">
          <div
            className="h-4 rounded-l-full bg-primary-400"
            style={{ width: `${(assetRatio.cash / (assetRatio.cash + assetRatio.stock)) * 100}%` }}
          ></div>
          <div
            className="h-4 rounded-r-full bg-[#00C49F]"
            style={{ width: `${(assetRatio.stock / (assetRatio.cash + assetRatio.stock)) * 100}%` }}
          ></div>
        </div>
        <div className="space-y-1">
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-primary-400"></div>
              <div>현금</div>
            </div>
            <div className="flex items-center gap-1.5 font-semibold">
              <span>{assetRatio.cash.toLocaleString()}원</span>
              <span className="text-sm text-muted-foreground ">
                ({((assetRatio.cash / (assetRatio.cash + assetRatio.stock)) * 100).toFixed(2)}%)
              </span>
            </div>
          </div>
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-[#00C49F]"></div>
              <div>주식</div>
            </div>
            <div className="flex items-center gap-1.5 font-semibold">
              <span>{assetRatio.stock.toLocaleString()}원</span>
              <span className="text-sm text-muted-foreground ">
                ({((assetRatio.stock / (assetRatio.cash + assetRatio.stock)) * 100).toFixed(2)}%)
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default MyAsset;
