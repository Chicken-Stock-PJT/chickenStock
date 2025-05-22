import { useState } from "react";
import {
  DropdownMenuContent,
  DropdownMenuSeparator,
  DropdownMenu,
  DropdownMenuLabel,
  DropdownMenuTrigger,
  DropdownMenuItem,
  DropdownMenuGroup,
} from "@/shared/libs/ui/dropdown-menu";
import { Link } from "react-router-dom";
import { SIDEBAR_CONFIG } from "@/widgets/sidebar/model/config";
import { useAuthStore } from "@/shared/store/auth";
import { LogOut } from "lucide-react";
import { LogoutModal } from "./ui/LogoutModal";

const HeaderDropdown = ({ nickname }: { nickname: string }) => {
  const [open, setOpen] = useState(false);
  const [showLogoutModal, setShowLogoutModal] = useState(false);
  const dropdownMenu = SIDEBAR_CONFIG;
  const logout = useAuthStore((state) => state.logout);

  const handleLogoutClick = () => {
    setOpen(false);
    setShowLogoutModal(true);
  };

  const handleLogoutConfirm = () => {
    // 비동기 함수를 래핑해서 void 반환
    const doLogout = async () => {
      await logout();
      setShowLogoutModal(false);
      window.location.reload();
    };

    void doLogout();
  };

  const handleLogoutCancel = () => {
    setShowLogoutModal(false);
  };

  return (
    <div>
      <DropdownMenu open={open} onOpenChange={setOpen}>
        <DropdownMenuTrigger className="bg-white font-semibold hover:text-primary-400">
          {nickname} <span>님</span>
        </DropdownMenuTrigger>
        <DropdownMenuContent>
          <DropdownMenuLabel>
            {nickname} <span className="font-normal">님</span>
          </DropdownMenuLabel>
          {dropdownMenu.map((menu) => (
            <DropdownMenuGroup key={menu.group}>
              <DropdownMenuSeparator />
              {menu.menus.map((menu) => (
                <DropdownMenuItem key={menu.title} onClick={() => setOpen(false)}>
                  <Link to={menu.url}>
                    <span className="flex items-center">
                      <menu.icon className="mr-2 size-4" /> {menu.title}
                    </span>
                  </Link>
                </DropdownMenuItem>
              ))}
            </DropdownMenuGroup>
          ))}
          <DropdownMenuSeparator />
          <DropdownMenuItem
            onClick={handleLogoutClick}
            className="cursor-pointer hover:text-primary-400"
          >
            <LogOut className="size-4" />
            <span>로그아웃</span>
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>

      {/* 로그아웃 확인 모달 */}
      <LogoutModal
        show={showLogoutModal}
        onConfirm={handleLogoutConfirm}
        onCancel={handleLogoutCancel}
      />
    </div>
  );
};

export default HeaderDropdown;
