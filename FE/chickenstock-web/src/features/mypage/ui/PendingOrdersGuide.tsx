import { Info } from "lucide-react";

const PendingOrdersGuide = () => {
  return (
    <div className="mt-6 rounded-lg bg-blue-50 p-4">
      <div className="flex items-start gap-3">
        <Info className="mt-0.5 size-5 text-blue-600" />
        <div className="space-y-2 text-sm text-blue-900">
          <p className="font-semibold">지정가 주문 안내</p>
          <ul className="list-inside list-disc space-y-1 text-blue-800">
            <li>지정가 주문은 원하는 가격에 자동으로 매수/매도가 체결됩니다.</li>
            <li>시장가가 목표가에 도달하면 주문이 실행됩니다.</li>
            <li>주문을 취소하려면 우측 X 버튼을 클릭하세요.</li>
            <li>체결된 주문은 거래내역에서 확인할 수 있습니다.</li>
          </ul>
        </div>
      </div>
    </div>
  );
};

export default PendingOrdersGuide;
