import { Outlet } from "react-router-dom";
import { MySidebar } from "@/widgets/sidebar/ui/MySideBar";

export const MyPageLayout = () => {
  return (
    <div className="absolute left-0 top-[56px] flex w-screen overflow-hidden overflow-x-auto">
      <aside>
        <MySidebar />
      </aside>
      <main className="mt-2 flex-1 p-6 ">
        <Outlet />
      </main>
    </div>
  );
};
