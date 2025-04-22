import { Search } from "lucide-react";
import { Link, NavLink } from "react-router-dom";
import { useState } from "react";
import logo from "../assets/logoImg.svg";

const Header = () => {
  const [isSearchOpen, setIsSearchOpen] = useState(false);

  return (
    <div className="flex items-center h-14 px-4 w-screen">
      <div className="flex items-center w-1/3">
        <img
          src={logo}
          alt="Chicken Stock Logo"
          className="h-7 w-7 mr-2 fill-current text-primary-400"
        />
        <Link to="/">
          <h1 className="text-lg font-bold">Chicken Stock</h1>
        </Link>
      </div>

      <div className="flex justify-center w-1/3">
        <nav className="flex space-x-6 items-center">
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
              className={`flex items-center text-gray-800 bg-gray-100 rounded-full transition-all duration-300 ease-in-out ${
                isSearchOpen ? "w-64" : "w-10"
              }`}
            >
              <Search
                size={28}
                onClick={() => setIsSearchOpen(!isSearchOpen)}
                className="p-1.5 rounded-full text-gray-400 hover:text-primary transition-colors duration-200"
              />
              <input
                type="text"
                placeholder="검색어를 입력하세요"
                className={`bg-transparent outline-none ml-3 ${
                  isSearchOpen ? "w-48 opacity-100" : "w-0 opacity-0"
                } transition-all duration-300`}
              />
            </div>
          </div>
        </nav>
      </div>

      <div className="flex justify-end w-1/3">auth</div>
    </div>
  );
};

export default Header;
