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

const HeaderDropdown = ({ nickname }: { nickname: string }) => {
  const dropdownMenu = SIDEBAR_CONFIG;
  const logout = useAuthStore((state) => state.logout);
  return (
    <div>
      <DropdownMenu>
        <DropdownMenuTrigger className="focus:none bg-white font-semibold hover:text-primary-400">
          {nickname} <span className="font-normal">님</span>
        </DropdownMenuTrigger>
        <DropdownMenuContent>
          <DropdownMenuLabel>
            {nickname} <span className="font-normal">님</span>
          </DropdownMenuLabel>
          {dropdownMenu.map((menu) => (
            <DropdownMenuGroup key={menu.group}>
              <DropdownMenuSeparator />
              {menu.menus.map((menu) => (
                <DropdownMenuItem key={menu.title}>
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
            onClick={() => void logout()}
            className="cursor-pointer hover:text-primary-400"
          >
            <LogOut className="size-4" />
            <span>로그아웃</span>
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </div>
  );
};

export default HeaderDropdown;
