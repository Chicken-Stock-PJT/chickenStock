import { createBrowserRouter } from "react-router-dom";
import ErrorPage from "@/pages/ErrorPage";
import HomePage from "@/pages/HomePage";
import Layout from "../Layout";
import StockPage from "@/pages/StockPage";
import StockList from "@/pages/StockList";
import Profile from "@/features/mypage/ProfileLayout";
import MyPage from "@/pages/mypage";
import ProfileEdit from "@/features/mypage/ProfileEdit";
import Portfolio from "@/features/mypage/Portfolio";
import Watchlist from "@/features/mypage/Watchlist";
import PendingOrders from "@/features/mypage/pending-orders";
import Transactions from "@/features/mypage/Transactions";
import SignupPage from "@/pages/SignupPage";
import LoginPage from "@/pages/LoginPage";
import LoginRedirect from "@/pages/LoginRedirect";
import FindPasswordPage from "@/pages/FindPasswordPage";
import ProtectedRoute from "./ProtectedRoute";
import AiBotPage from "@/pages/AiBotPage";

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
        children: [
          {
            path: "",
            element: <LoginPage />,
          },
          {
            path: "redirect",
            element: <LoginRedirect />,
          },
        ],
      },
      {
        path: "signup",
        element: <SignupPage />,
      },
      // 비밀번호찾기
      {
        path: "find",
        children: [
          {
            path: "password",
            element: <FindPasswordPage />,
          },
        ],
      },
      {
        path: "mypage",
        element: (
          <ProtectedRoute>
            <MyPage />
          </ProtectedRoute>
        ),
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
          {
            path: "pending-orders",
            element: <PendingOrders />,
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
        path: "ai-bot",
        element: <AiBotPage />,
      },
    ],
  },
]);

export default router;
