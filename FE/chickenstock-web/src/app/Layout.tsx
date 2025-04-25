import { Outlet } from "react-router-dom";
import Header from "../widgets/Header";

function Layout() {
  return (
    <div className="min-h-screen">
      <Header />
      <div className="mx-auto mt-6 max-w-[1200px]">
        <Outlet />
      </div>
    </div>
  );
}

export default Layout;
