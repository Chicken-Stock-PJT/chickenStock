import { createBrowserRouter } from "react-router-dom";
import ErrorPage from "../../pages/ErrorPage";
import HomePage from "../../pages/HomePage";
import Layout from "../Layout";
import StockPage from "../../pages/StockPage";
// import LoginPage from "../../pages/LoginPage";
// import SignupPage from "../../pages/SignupPage";
// import DashboardPage from "../../pages/DashboardPage";

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
      //   {
      //     path: "login",
      //     element: <LoginPage />,
      //   },
      //   {
      //     path: "signup",
      //     element: <SignupPage />,
      //   },
      //   {
      //     path: "dashboard",
      //     element: <DashboardPage />,
      //   },
      {
        path: "stocks",
        element: <StockPage />,
      },
    ],
  },
]);

export default router;
