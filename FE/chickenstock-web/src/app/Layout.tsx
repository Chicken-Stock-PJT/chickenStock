import { Outlet } from "react-router-dom";
import Header from "../widgets/Header";
import { Toaster } from "@/shared/libs/ui/toaster";
function Layout() {
  return (
    <div className="min-h-screen">
      <Header />
      <div className="mx-auto mt-6 max-w-[1200px]">
        <Outlet />
      </div>
      <Toaster />
    </div>
  );
}

export default Layout;
