import { Outlet } from "react-router-dom";
import Header from "../widgets/Header";

function Layout() {
  return (
    <div className="min-h-screen">
      <Header />
      <div className="max-w-[1200px] mx-auto mt-6">
        <Outlet />
      </div>
    </div>
  );
}

export default Layout;
