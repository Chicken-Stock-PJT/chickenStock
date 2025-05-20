import { X, Home, LineChart, Bot, Trophy, LogOut } from "lucide-react";
import { useNavigate } from "react-router-dom";
import { SIDEBAR_CONFIG } from "@/widgets/sidebar/model/config";
import { useAuthStore } from "@/shared/store/auth";
interface SideNavbarProps {
  onClick: () => void;
  onRankingOpen: () => void;
}

const SideNavbar = ({ onClick, onRankingOpen }: SideNavbarProps) => {
  const navigate = useNavigate();

  const handleNavigation = (path: string) => {
    onClick();
    void navigate(path);
  };

  const handleLogout = async () => {
    await useAuthStore.getState().logout();
    onClick();
    void navigate("/");
  };

  return (
    <div className="fixed inset-0 z-40">
      {/* 배경 오버레이 */}
      <div
        className="absolute inset-0 bg-black/50 transition-opacity duration-300"
        onClick={onClick}
      />

      {/* 사이드바 컨테이너 */}
      <nav className="absolute right-0 h-full w-[280px] bg-white shadow-lg transition-transform duration-300">
        {/* 헤더 */}

        {/* 네비게이션 메뉴 */}
        <div className="flex flex-col gap-1 p-4 text-left">
          <div className="mb-2 flex items-center justify-between">
            <h2 className="px-4 text-sm font-medium text-gray-500">메뉴</h2>
            <X className="size-5" onClick={onClick} />
          </div>
          <button
            onClick={() => handleNavigation("/")}
            className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            <Home className="size-5" />
            <span className="font-medium">홈</span>
          </button>
          <button
            onClick={() => handleNavigation("/stocks/005930")}
            className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            <LineChart className="size-5" />
            <span className="font-medium">주식</span>
          </button>
          <button
            onClick={() => handleNavigation("/ai-bot")}
            className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            <Bot className="size-5" />
            <span className="font-medium">AI봇</span>
          </button>
          <button
            onClick={onRankingOpen}
            className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            <Trophy className="size-5" />
            <span className="font-medium">랭킹</span>
          </button>
        </div>

        {SIDEBAR_CONFIG.map((group) => (
          <div key={group.group} className="flex flex-col gap-1 p-4 text-left">
            <h2 className="mb-2 px-4 text-sm font-medium text-gray-500">{group.group}</h2>
            {group.menus.map((menu) => (
              <button
                key={menu.title}
                onClick={() => handleNavigation(menu.url)}
                className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
              >
                <menu.icon className="size-5" />
                <span className="font-medium">{menu.title}</span>
              </button>
            ))}
          </div>
        ))}

        <div className="flex flex-col gap-1 p-4 text-left">
          <button
            onClick={handleLogout}
            className="flex items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            <LogOut className="size-5" />
            <span className="font-medium">로그아웃</span>
          </button>
        </div>
      </nav>
    </div>
  );
};

export default SideNavbar;
