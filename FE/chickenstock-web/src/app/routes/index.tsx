import { createBrowserRouter } from "react-router-dom";
import ErrorPage from "@/pages/ErrorPage";
import HomePage from "@/pages/HomePage";
import Layout from "../Layout";
import StockPage from "@/pages/StockPage";
import StockList from "@/pages/StockList";
import Profile from "@/features/mypage/Profile";
import MyPage from "@/pages/mypage";
import ProfileEdit from "@/features/mypage/ProfileEdit";
import Portfolio from "@/features/mypage/Portfolio";
import Watchlist from "@/features/mypage/Watchlist";
import Transactions from "@/features/mypage/Transactions";
import SignupPage from "@/pages/SignupPage";
import LoginPage from "@/pages/LoginPage";
import LoginRedirect from "@/pages/LoginRedirect";

const router = createBrowserRouter([
  {
    path: "/",
    element: <Layout />,
    errorElement: <ErrorPage />,
    children: [
      {
        path: "",
        element: <HomePage />,
      },
      {
        path: "login",
        element: <LoginPage />,
      },
      {
        path: "signup",
        element: <SignupPage />,
      },
      {
        path: "mypage",
        element: <MyPage />,
        children: [
          {
            path: "",
            element: <Profile />,
          },
          {
            path: "edit",
            element: <ProfileEdit />,
          },
          // 커뮤니티 기능 추가 시 주석 해제
          // {
          //   path: "history",
          //   element: <History />,
          // },
          {
            path: "portfolio",
            element: <Portfolio />,
          },
          {
            path: "transactions",
            element: <Transactions />,
          },
          {
            path: "watchlist",
            element: <Watchlist />,
          },
        ],
      },
      {
        path: "stocks",
        children: [
          {
            path: "",
            element: <StockList />,
          },
          {
            path: ":stockCode",
            element: <StockPage />,
          },
        ],
      },
      {
        path: "login/redirect",
        element: <LoginRedirect />,
      },
    ],
  },
]);

export default router;
