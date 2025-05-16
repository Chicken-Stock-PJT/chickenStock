import { Button } from "@/shared/libs/ui/button";
import logo from "@/assets/logoImg.svg";
import { useState } from "react";
import InvestmentGuideModal from "./InvestmentGuideModal";

const Banner = () => {
  const [isModalOpen, setIsModalOpen] = useState(false);

  const openModal = () => {
    setIsModalOpen(true);
  };

  const closeModal = () => {
    setIsModalOpen(false);
  };

  return (
    <section className="bg-gradient-to-b from-yellow-50 to-white py-16">
      <div className="container mx-auto max-w-[1200px] px-4">
        <div className="flex flex-col items-center gap-8 md:flex-row">
          <div className="space-y-6 text-left md:w-1/2">
            <h1 className="text-4xl/2 font-bold leading-tight md:text-5xl">
              투자가 처음이라면,
              <br />
              <span className="mt-2 block text-yellow-600">
                ChickenStock
                <span className="text-black">과 함께</span>
              </span>
            </h1>
            <p className="text-lg text-muted-foreground">
              실제 투자가 두려운 분들을 위한 모의투자 플랫폼. <br />
              <span className="text-lg text-muted-foreground">
                재미있게 투자를 배우고 실전 감각을 키워보세요.
              </span>
            </p>

            <Button size="lg" className="font-bold" onClick={openModal}>
              모의투자 시작 가이드
            </Button>
          </div>
          <div className="mx-auto md:w-1/3">
            <img src={logo} alt="ChickenStock 모의투자 플랫폼" />
          </div>
        </div>
      </div>

      <InvestmentGuideModal isOpen={isModalOpen} onClose={closeModal} />
    </section>
  );
};

export default Banner;
