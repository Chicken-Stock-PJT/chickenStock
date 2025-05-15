import { useState, useEffect } from "react";
import { ChevronUp } from "lucide-react";

function GoToTop() {
  const [isVisible, setIsVisible] = useState(false);

  // 스크롤 위치에 따라 버튼 표시/숨김
  useEffect(() => {
    const toggleVisibility = () => {
      if (window.pageYOffset > 300) {
        setIsVisible(true);
      } else {
        setIsVisible(false);
      }
    };

    window.addEventListener("scroll", toggleVisibility);
    return () => window.removeEventListener("scroll", toggleVisibility);
  }, []);

  const scrollToTop = () => {
    window.scrollTo({
      top: 0,
      behavior: "smooth",
    });
  };

  return (
    <>
      {isVisible && (
        <button
          onClick={scrollToTop}
          className="fixed bottom-32 right-5 z-50 rounded-full p-3 bg-gray-700 text-white shadow-lg transition-all hover:bg-gray-800 hover:shadow-xl"
          aria-label="페이지 최상단으로 이동"
        >
          <ChevronUp className="size-5" />
        </button>
      )}
    </>
  );
}

export default GoToTop;
