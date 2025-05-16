// InvestmentGuideModal.tsx
import { useState } from "react";
import { Button } from "@/shared/libs/ui/button";
import { X } from "lucide-react";

interface InvestmentGuideModalProps {
  isOpen: boolean;
  onClose: () => void;
}

const InvestmentGuideModal = ({ isOpen, onClose }: InvestmentGuideModalProps) => {
  const [activeTab, setActiveTab] = useState("basic");

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="relative max-h-[90vh] w-full max-w-3xl overflow-auto rounded-lg bg-white p-6 shadow-xl">
        <button
          onClick={onClose}
          className="absolute right-4 top-4 text-gray-500 hover:text-gray-700"
        >
          <X size={24} />
        </button>

        <h2 className="mb-6 text-center text-2xl font-bold text-yellow-600">
          ChickenStock 모의투자 가이드
        </h2>

        <div className="mb-6 flex border-b">
          <button
            className={`flex-1 py-2 text-center font-medium ${
              activeTab === "basic"
                ? "border-b-2 border-yellow-600 text-yellow-600"
                : "text-gray-500"
            }`}
            onClick={() => setActiveTab("basic")}
          >
            기본 투자 전략
          </button>
          <button
            className={`flex-1 py-2 text-center font-medium ${
              activeTab === "trading"
                ? "border-b-2 border-yellow-600 text-yellow-600"
                : "text-gray-500"
            }`}
            onClick={() => setActiveTab("trading")}
          >
            거래 시간 안내
          </button>
          <button
            className={`flex-1 py-2 text-center font-medium ${
              activeTab === "fees"
                ? "border-b-2 border-yellow-600 text-yellow-600"
                : "text-gray-500"
            }`}
            onClick={() => setActiveTab("fees")}
          >
            수수료 안내
          </button>
        </div>

        {activeTab === "basic" && (
          <div className="space-y-4">
            <div className="rounded-lg bg-yellow-50 p-4">
              <h3 className="mb-2 text-left font-bold text-yellow-700">분산 투자의 중요성</h3>
              <p className="text-left">
                모든 주식을 한 종목에 투자하지 마세요. 다양한 산업군의 주식에 분산 투자함으로써
                리스크를 줄일 수 있습니다.
              </p>
            </div>

            <div className="rounded-lg bg-blue-50 p-4">
              <h3 className="mb-2 text-left font-bold text-blue-700">장기 투자 마인드</h3>
              <p className="text-left">
                단기적인 주가 변동에 일희일비하지 마세요. 기업의 펀더멘털을 분석하고 장기적인
                관점에서 투자하는 것이 중요합니다.
              </p>
            </div>

            <div className="rounded-lg bg-green-50 p-4">
              <h3 className="mb-2 text-left font-bold text-green-700">투자 전 기업 분석</h3>
              <p className="text-left">
                기업의 재무제표, 시장 점유율, 성장 가능성 등을 분석해보세요. ChickenStock에서는 기업
                정보를 쉽게 확인할 수 있습니다.
              </p>
            </div>

            <div className="rounded-lg bg-purple-50 p-4">
              <h3 className="mb-2 text-left font-bold text-purple-700">정기적인 투자</h3>
              <p className="text-left">
                일정 금액을 정기적으로 투자하는 적립식 투자는 시장 타이밍을 맞추기 어려운 초보
                투자자에게 좋은 전략입니다.
              </p>
            </div>

            <div className="mt-6 text-center">
              <Button
                className="bg-yellow-300 hover:bg-yellow-600"
                onClick={() => setActiveTab("trading")}
              >
                거래 시간 확인하기
              </Button>
            </div>
          </div>
        )}

        {activeTab === "trading" && (
          <div className="space-y-4">
            <div className="overflow-x-auto">
              <table className="w-full table-auto border-collapse">
                <thead>
                  <tr className="bg-gray-100">
                    <th className="border p-2 text-center">구분</th>
                    <th className="border p-2 text-center">주문 가능 시간</th>
                    <th className="border p-2 text-center">주문 가능 종목</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td className="border p-2">프리마켓</td>
                    <td className="border p-2">08:00-08:50</td>
                    <td className="border p-2">일부 종목 한정</td>
                  </tr>
                  <tr>
                    <td className="border p-2">정규장 (메인마켓)</td>
                    <td className="border p-2">09:00-15:20</td>
                    <td className="border p-2">모든 종목 가능</td>
                  </tr>
                  <tr>
                    <td className="border p-2">애프터마켓</td>
                    <td className="border p-2">15:30-20:00</td>
                    <td className="border p-2">일부 종목 한정</td>
                  </tr>
                </tbody>
              </table>
            </div>

            <div className="rounded-lg bg-gray-50 p-4 text-sm">
              <h3 className="mb-2 font-medium">주요 참고사항:</h3>
              <ul className="space-y-2">
                <li>• 넥스트레이드(NXT) 휴장: 08:50-09:00, 15:20-15:30</li>
                <li>
                  • 09:00-15:20 동안 한국거래소(KRX)와 넥스트레이드(NXT)의 시세를 비교해서 매수
                  기회성이 높은 거래소로 주문이 전환됩니다.
                </li>
                <li>
                  • 정규장 (메인마켓) 시간부터 주문은 애프터마켓이 끝나는 20:00까지 유지됩니다.
                </li>
                <li>• 단, 개별 시장의 직전 선택했던 선택할 시장을 우선으로 주문이 전환됩니다.</li>
              </ul>
            </div>

            <div className="mt-6 text-center">
              <Button
                className="bg-yellow-300 hover:bg-yellow-600"
                onClick={() => setActiveTab("fees")}
              >
                수수료 안내 확인하기
              </Button>
            </div>
          </div>
        )}

        {activeTab === "fees" && (
          <div className="space-y-4">
            <div className="overflow-x-auto">
              <table className="w-full table-auto border-collapse">
                <thead>
                  <tr className="bg-gray-100">
                    <th className="border p-2 text-center">거래 유형</th>
                    <th className="border p-2 text-center">수수료율</th>
                    <th className="border p-2 text-center">비고</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td className="border p-2">국내 주식 매수</td>
                    <td className="border p-2">0.015%</td>
                    <td className="border p-2">실제 증권사 대비 저렴한 요율</td>
                  </tr>
                  <tr>
                    <td className="border p-2">국내 주식 매도</td>
                    <td className="border p-2">0.05% + 제세금 0.18%</td>
                    <td className="border p-2">양도소득세 별도</td>
                  </tr>
                </tbody>
              </table>
            </div>

            <div className="rounded-lg bg-yellow-50 p-4">
              <h3 className="mb-2 font-bold text-yellow-700">ChickenStock 모의투자 혜택</h3>
              <p>
                실제 증권사보다 낮은 수수료로 더 많은 투자 경험을 쌓을 수 있습니다. 모의투자지만
                실제와 동일한 수수료 환경을 제공하여 현실적인 투자 감각을 키워보세요.
              </p>
            </div>

            <div className="rounded-lg bg-gray-50 p-4 text-sm">
              <h3 className="mb-2 font-medium">알아두세요:</h3>
              <ul className="space-y-2">
                <li>• 수수료는 거래금액에 비례하여 계산됩니다.</li>
                <li>• 모의투자 환경에서는 실제 돈이 오가지 않지만, 수수료가 계산에 반영됩니다.</li>
                <li>• VIP 등급에 따라 수수료 우대 혜택이 적용될 수 있습니다.</li>
              </ul>
            </div>

            <div className="mt-6 text-center">
              <Button className="bg-yellow-300 hover:bg-yellow-600" onClick={onClose}>
                모의투자 시작하기
              </Button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default InvestmentGuideModal;
