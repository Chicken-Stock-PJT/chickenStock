import { matchPath, Outlet, useLocation } from "react-router-dom";
import Header from "../widgets/Header";
import { Toaster } from "@/shared/libs/ui/toaster";
function Layout() {
  const { pathname } = useLocation();
  const isStockPage = !!matchPath("/stocks/:stockCode", pathname);

  return (
    <div className="min-h-screen">
      <Header />
      <div className={isStockPage ? "mt-6 w-full" : "mx-auto mt-6 max-w-[1200px]"}>
        <Outlet />
      </div>
      <Toaster />
    </div>
  );
}

export default Layout;
