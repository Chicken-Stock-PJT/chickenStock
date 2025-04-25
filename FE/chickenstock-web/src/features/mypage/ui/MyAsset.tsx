import { ChevronRight } from "lucide-react";

const MyAsset = () => {
  return (
    <div className="space-y-6 rounded-xl border p-6 text-left shadow">
      <div className="space-y-4">
        <div className="text-xl font-semibold">자산 비율</div>
        {/* <div className="flex items-center justify-between">
            <span className="text-xs text-muted-foreground">저빈도</span>
            <span className="text-xs text-muted-foreground">고빈도</span>
          </div> */}
        <div className="flex h-4 w-full rounded-full bg-gray-200">
          <div className="h-4 rounded-l-full bg-primary-400" style={{ width: "20%" }}></div>
          <div className="h-4 rounded-r-full bg-[#00C49F]" style={{ width: "80%" }}></div>
        </div>
        <div>
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-primary-400"></div>
              <div>현금</div>
            </div>
            <div className="flex gap-1.5">
              <span className="font-semibold">10,000,000원</span>
              {/* <ChevronRight className="text-gray-400 opacity-0" /> */}
            </div>
          </div>
          <div className="flex flex-1 justify-between">
            <div className="justify-left flex items-center gap-3">
              <div className="size-[12px] rounded-sm bg-[#00C49F]"></div>
              <div>주식</div>
            </div>
            <div className="flex gap-1.5">
              <span className="font-semibold">2,500,000원</span>
              {/* <ChevronRight className="text-gray-400" /> */}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default MyAsset;
