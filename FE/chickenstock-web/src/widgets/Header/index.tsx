import { Search } from "lucide-react";
import { Link, NavLink } from "react-router-dom";
import { useState } from "react";
import logo from "../../assets/logoImg.svg";
import SearchModal from "@/features/stocks/search/ui/SearchModal";
import RankingModal from "@/widgets/Header/ui/RankingModal";
import { useAuthStore } from "@/shared/store/auth";
import HeaderDropdown from "@/widgets/Header/HeaderDropdown";
import { useSimpleProfile } from "@/shared/model/queries";

const Header = () => {
  const [isSearchOpen, setIsSearchOpen] = useState(false);
  const [isRankingOpen, setIsRankingOpen] = useState(false);
  const { data: simpleProfile } = useSimpleProfile();
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return (
    <div className="sticky top-0 z-50 flex h-14 w-screen items-center justify-between bg-white px-4 md:px-6 lg:px-8">
      <div className="flex items-center lg:mr-[200px]">
        <img
          src={logo}
          alt="Chicken Stock Logo"
          className="mr-2 size-7 fill-current text-primary-400"
        />
        <Link to="/">
          <h1 className="text-lg font-bold">ChickenStock</h1>
        </Link>
      </div>

      <div className="hidden flex-1 items-center justify-center md:flex">
        <nav className="flex items-center space-x-4 lg:space-x-6">
          <NavLink
            to="/"
            className={({ isActive }) =>
              `${
                isActive ? "text-primary-400" : ""
              } text-base font-semibold transition-colors duration-200`
            }
          >
            홈
          </NavLink>
          <NavLink
            to="stocks"
            className={({ isActive }) =>
              `${
                isActive ? "text-primary-400" : ""
              } text-base font-semibold transition-colors duration-200`
            }
          >
            주식
          </NavLink>
          {/* 랭킹 버튼 추가 */}
          <div
            onClick={() => setIsRankingOpen(true)}
            className="cursor-pointer text-base font-semibold transition-colors duration-200 hover:text-primary-400"
          >
            랭킹
          </div>
          <div className="relative">
            <div
              className={`flex items-center rounded-full bg-gray-100 text-gray-800 transition-all duration-300 ease-in-out ${
                isSearchOpen ? "w-48 md:w-56 lg:w-64" : "w-10"
              }`}
            >
              <Search
                size={28}
                onClick={() => setIsSearchOpen(!isSearchOpen)}
                className="cursor-pointer rounded-full p-1.5 text-gray-400 transition-colors duration-200 hover:text-primary"
              />
            </div>
          </div>
        </nav>
      </div>

      {isLogin ? (
        <div className="flex items-center text-sm font-semibold lg:text-lg">
          <div className="flex items-center gap-4">
            <div className="hidden items-center gap-2 lg:flex">
              <div className="rounded-md bg-primary-100 p-2 text-xs">총 자산</div>
              <div className="text-sm">₩{Number(simpleProfile?.totalAsset).toLocaleString()}</div>
            </div>
            <div className="hidden items-center gap-2 lg:flex">
              <div className="rounded-md bg-primary-100 p-2 text-xs">수익률</div>
              <div
                className={`text-sm ${Number(simpleProfile?.returnRate) > 0 ? "text-chart-red" : Number(simpleProfile?.returnRate) < 0 ? "text-chart-blue" : ""}`}
              >
                {Number(simpleProfile?.returnRate) > 0 ? "+" : ""}
                {Number(simpleProfile?.returnRate).toFixed(2)}%
              </div>
            </div>
          </div>
          <HeaderDropdown nickname={simpleProfile?.nickname ?? ""} />
        </div>
      ) : (
        <div className="flex items-center gap-4 lg:gap-10">
          <Link to="/login" className="text-sm font-semibold lg:text-base">
            로그인
          </Link>
          <Link to="/signup" className="text-sm font-semibold lg:text-base">
            회원가입
          </Link>
        </div>
      )}
      <SearchModal open={isSearchOpen} onOpenChange={setIsSearchOpen} />
      <RankingModal open={isRankingOpen} onOpenChange={setIsRankingOpen} />
    </div>
  );
};

export default Header;
