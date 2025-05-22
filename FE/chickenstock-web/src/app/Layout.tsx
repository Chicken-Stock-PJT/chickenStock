import { Outlet } from "react-router-dom";
import Header from "../widgets/Header";
import { Toaster } from "@/shared/libs/ui/toaster";
import ChatNotification from "../widgets/ChatNotification/ui";
import { useWebSocket } from "@/widgets/ChatNotification/hooks/useWebSocket";
import GoToTop from "@/widgets/GoToTop";
import { useEffect } from "react";
import { initializeNxtStocks } from "@/features/stocks/trade/model/nxtStocks";

function Layout() {
  // 여기서 WebSocket 연결을 관리
  useWebSocket();
  useEffect(() => {
    void initializeNxtStocks();
  }, []);
  return (
    <div className="min-h-screen">
      <Header />
      <div className="mx-auto mt-6 max-w-[1200px]">
        <Outlet />
      </div>
      <Toaster />
      <GoToTop />
      <ChatNotification />
    </div>
  );
}

export default Layout;
