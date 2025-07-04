// src/widgets/my-sidebar/ui/MySidebar.tsx
import { useLocation, Link } from "react-router-dom";
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
} from "@/shared/libs/ui/sidebar";
import { SIDEBAR_CONFIG } from "../model/config";

export const MySidebar = () => {
  const { pathname } = useLocation();

  return (
    <SidebarProvider>
      <Sidebar>
        <SidebarContent>
          {SIDEBAR_CONFIG.map((item, idx) => (
            <SidebarGroup key={idx}>
              <SidebarGroupLabel>{item.group}</SidebarGroupLabel>
              <SidebarGroupContent>
                <SidebarMenu>
                  {item.menus.map((menu, menuIdx) => (
                    <SidebarMenuItem key={menuIdx}>
                      <SidebarMenuButton asChild isActive={pathname === menu.url}>
                        <Link to={menu.url}>
                          <menu.icon className="mr-2 size-4" />
                          {menu.title}
                        </Link>
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
  );
};
