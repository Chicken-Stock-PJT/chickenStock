import { Search } from "lucide-react";
import { Link, NavLink, useNavigate } from "react-router-dom";
import { useState } from "react";
import logo from "../assets/logoImg.svg";

const Header = () => {
  const [isSearchOpen, setIsSearchOpen] = useState(false);
  const navigate = useNavigate();

  return (
    <div className="flex h-14 w-screen items-center px-4">
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
                className="rounded-full p-1.5 text-gray-400 transition-colors duration-200 hover:text-primary"
              />
              <input
                type="text"
                placeholder="검색어를 입력하세요"
                className={`ml-3 bg-transparent outline-none ${
                  isSearchOpen ? "w-48 opacity-100" : "w-0 opacity-0"
                } transition-all duration-300`}
              />
            </div>
          </div>
        </nav>
      </div>

      <div className="flex w-1/3 justify-end" onClick={() => void navigate("mypage/profile")}>
        auth
      </div>
    </div>
  );
};

export default Header;
