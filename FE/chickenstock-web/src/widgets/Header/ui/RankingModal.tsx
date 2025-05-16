import { useState } from "react";
import { Modal, ModalContent, ModalHeader, ModalTitle } from "@/shared/libs/ui/modal";
import TotalAssetRankingList from "./TotalAssetRankingList";
import ReturnRateRankingList from "./ReturnRateRankingList";
import { RankingType } from "../model/types";

interface RankingModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

const RankingModal = ({ open, onOpenChange }: RankingModalProps) => {
  const [activeTab, setActiveTab] = useState<RankingType>("totalAsset");

  return (
    <Modal open={open} onOpenChange={onOpenChange}>
      <ModalContent className="max-h-[90vh] max-w-md overflow-y-auto rounded-xl bg-gradient-to-b from-white to-gray-50 shadow-lg">
        <ModalHeader className="border-b border-gray-100 pb-3">
          <ModalTitle className="flex items-center gap-2 text-xl font-bold text-gray-800">
            <div className="size-5 text-yellow-500" />
            자산 랭킹
          </ModalTitle>
        </ModalHeader>

        {/* 탭 네비게이션 */}
        <div className="mx-4 mt-4 flex border-b border-gray-200">
          <button
            className={`flex-1 px-4 py-2 text-center font-medium ${
              activeTab === "totalAsset"
                ? "border-b-2 border-orange-500 text-orange-600"
                : "text-gray-500 hover:text-gray-700"
            }`}
            onClick={() => setActiveTab("totalAsset")}
          >
            총 자산
          </button>
          <button
            className={`flex-1 px-4 py-2 text-center font-medium ${
              activeTab === "returnRate"
                ? "border-b-2 border-orange-500 text-orange-600"
                : "text-gray-500 hover:text-gray-700"
            }`}
            onClick={() => setActiveTab("returnRate")}
          >
            수익률
          </button>
        </div>

        {/* 랭킹 리스트 */}
        {activeTab === "totalAsset" ? <TotalAssetRankingList /> : <ReturnRateRankingList />}
      </ModalContent>
    </Modal>
  );
};

export default RankingModal;
