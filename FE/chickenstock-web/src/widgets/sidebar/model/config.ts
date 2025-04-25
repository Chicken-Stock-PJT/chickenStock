import { User, Settings, Calendar, ChartPie, ScrollText, Heart } from "lucide-react";

export const SIDEBAR_CONFIG = [
  {
    group: "내 정보",
    menus: [
      { title: "프로필", url: "/mypage/profile", icon: User },
      { title: "정보수정", url: "/mypage/edit", icon: Settingsgit  },
      { title: "활동기록", url: "/mypage/history", icon: Calendar },
    ],
  },
  {
    group: "내 투자자",
    menus: [
      { title: "투자현황", url: "/mypage/portfolio", icon: ChartPie },
      { title: "거래내역", url: "/mypage/transactions", icon: ScrollText },
      { title: "관심종목", url: "/mypage/watchlist", icon: Heart },
    ],
  },
];
