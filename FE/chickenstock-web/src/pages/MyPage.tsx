import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarGroupContent,
  SidebarGroupLabel,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarProvider,
} from "@/widgets/ui/sidebar";
import { User, UserPen, Calendar, ChartPie, ScrollText, Heart } from "lucide-react";
import { Outlet, useLocation } from "react-router-dom";

const MyPage = () => {
  const { pathname } = useLocation();

  const items = [
    {
      group: "내 정보",
      menus: [
        {
          title: "프로필",
          url: "/mypage/profile",
          icon: User,
        },
        {
          title: "정보 수정",
          url: "/mypage/edit",
          icon: UserPen,
        },
        {
          title: "활동기록",
          url: "/mypage/history",
          icon: Calendar,
        },
      ],
    },
    {
      group: "내 투자자",
      menus: [
        {
          title: "투자현황",
          url: "/mypage/portfolio",
          icon: ChartPie,
        },
        {
          title: "거래내역",
          url: "/mypage/transactions",
          icon: ScrollText,
        },
        {
          title: "관심종목",
          url: "/mypage/watchlist",
          icon: Heart,
        },
      ],
    },
  ];

  return (
    <div className="absolute left-0 top-[56px] flex max-h-[calc(90vh)] w-screen overflow-hidden overflow-x-auto">
      <aside>
        <SidebarProvider>
          <Sidebar>
            <SidebarContent>
              {items.map((item, idx) => (
                <SidebarGroup key={idx}>
                  <SidebarGroupLabel className="text-md mb-2 font-semibold">
                    {item.group}
                  </SidebarGroupLabel>
                  <SidebarGroupContent>
                    <SidebarMenu>
                      {item.menus.map((menu) => (
                        <SidebarMenuItem key={menu.title}>
                          <SidebarMenuButton asChild isActive={pathname === menu.url}>
                            <a href={menu.url}>
                              <menu.icon />
                              <span>{menu.title}</span>
                            </a>
                          </SidebarMenuButton>
                        </SidebarMenuItem>
                      ))}
                    </SidebarMenu>
                  </SidebarGroupContent>
                </SidebarGroup>
              ))}
            </SidebarContent>
          </Sidebar>
        </SidebarProvider>
      </aside>
      <Outlet />
    </div>
  );
};

export default MyPage;
