import { Outlet } from "react-router-dom";
import { MySidebar } from "@/widgets/sidebar/ui/MySideBar";

export const MyPageLayout = () => {
  return (
    <div className="absolute left-0 top-[56px] flex max-h-[calc(90vh)] w-screen overflow-hidden overflow-x-auto">
      <aside>
        <MySidebar />
      </aside>
      <main className="flex-1 p-6 mt-2 ">
        <Outlet />
      </main>
    </div>
  );
};
