import { Search } from "lucide-react";
import { Link, NavLink } from "react-router-dom";
import { useState } from "react";
import logo from "../../assets/logoImg.svg";
import SearchModal from "@/features/stocks/search/ui/SearchModal";
import { useAuthStore } from "@/shared/store/auth";
import HeaderDropdown from "@/widgets/Header/HeaderDropdown";

const Header = () => {
  const [isSearchOpen, setIsSearchOpen] = useState(false);
  const { simpleProfile } = useAuthStore();
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return (
    <div className="sticky top-0 z-50 flex h-14 w-screen items-center bg-white px-4">
      <div className="flex w-1/3 items-center">
        <img
          src={logo}
          alt="Chicken Stock Logo"
          className="mr-2 size-7 fill-current text-primary-400"
        />
        <Link to="/">
          <h1 className="text-lg font-bold">ChickenStock</h1>
        </Link>
      </div>

      <div className="flex w-1/3 justify-center">
        <nav className="flex items-center space-x-6">
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
          <div className="relative">
            <div
              className={`flex items-center rounded-full bg-gray-100 text-gray-800 transition-all duration-300 ease-in-out ${
                isSearchOpen ? "w-64" : "w-10"
              }`}
            >
              <Search
                size={28}
                onClick={() => setIsSearchOpen(!isSearchOpen)}
                className=" cursor-pointer rounded-full p-1.5 text-gray-400 transition-colors duration-200 hover:text-primary"
              />
            </div>
          </div>
        </nav>
      </div>
      {isLogin ? (
        <div className="mr-10 flex w-1/3 items-center justify-end gap-8 text-lg font-semibold">
          <div className="flex items-center gap-2">
            <div className="rounded-md bg-primary-100 p-2 text-xs">잔고</div>
            <div className="text-sm">\{Number(simpleProfile?.memberMoney).toLocaleString()}</div>
          </div>
          <div className="flex items-center gap-2">
            <div className="rounded-md bg-primary-100 p-2 text-xs">수익률</div>
            <div
              className={`text-sm ${Number(simpleProfile?.returnRate) > 0 ? "text-chart-red" : Number(simpleProfile?.returnRate) < 0 ? "text-chart-blue" : ""}`}
            >
              {simpleProfile?.returnRate}%
            </div>
          </div>
          <HeaderDropdown nickname={simpleProfile?.nickname ?? ""} />
        </div>
      ) : (
        <div className="mr-10 flex w-1/3 justify-end gap-10">
          <Link to="/login" className="font-semibold">
            로그인
          </Link>
          <Link to="/signup" className="font-semibold">
            회원가입
          </Link>
        </div>
      )}
      <SearchModal open={isSearchOpen} onOpenChange={setIsSearchOpen} />
    </div>
  );
};

export default Header;
