import { X, Home, LineChart, Bot, Trophy, LogOut, LogIn } from "lucide-react";
import { useNavigate } from "react-router-dom";
import { SIDEBAR_CONFIG } from "@/widgets/sidebar/model/config";
import { useAuthStore } from "@/shared/store/auth";

interface SideNavbarProps {
  onClick: () => void;
  onRankingOpen: () => void;
}

const SideNavbar = ({ onClick, onRankingOpen }: SideNavbarProps) => {
  const navigate = useNavigate();
  const isLoggedIn = useAuthStore((state) => state.isLoggedIn);

  const handleNavigation = (path: string) => {
    onClick();
    void navigate(path);
  };

  const handleLogout = () => {
    void useAuthStore.getState().logout();
    onClick();
    void navigate("/");
  };

  return (
    <>
      {/* 배경 오버레이 */}
      <div
        className="fixed inset-0 z-40 bg-black/50 transition-opacity duration-300"
        onClick={onClick}
      />

      {/* 사이드바 컨테이너 */}
      <div className="fixed inset-y-0 right-0 z-50 flex w-[280px] flex-col overflow-auto bg-white shadow-lg">
        {/* 네비게이션 메뉴 */}
        <div className="flex flex-col gap-1 p-4 text-left">
          <div className="mb-2 flex items-center justify-between">
            <h2 className="px-4 text-sm font-medium text-gray-500">메뉴</h2>
            <button onClick={onClick} className="rounded-full p-1 hover:bg-gray-100">
              <X className="size-5" />
            </button>
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

        {/* 스크롤 가능한 영역 */}
        <div className="flex-1">
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
        </div>

        {/* 하단 고정 영역 */}
        <div className="border-t p-4">
          <button
            onClick={isLoggedIn ? handleLogout : () => handleNavigation("/login")}
            className="flex w-full items-center gap-3 rounded-lg px-4 py-3 text-gray-700 hover:bg-gray-50"
          >
            {isLoggedIn ? (
              <>
                <LogOut className="size-5" />
                <span className="font-medium">로그아웃</span>
              </>
            ) : (
              <>
                <LogIn className="size-5" />
                <span className="font-medium">로그인</span>
              </>
            )}
          </button>
        </div>
      </div>
    </>
  );
};

export default SideNavbar;
