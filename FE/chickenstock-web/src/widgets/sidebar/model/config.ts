import {
  User,
  Settings,
  ChartNoAxesCombined,
  ReceiptText,
  Heart,
  ClipboardList,
} from "lucide-react";

export const SIDEBAR_CONFIG = [
  {
    group: "내 정보",
    menus: [
      { title: "프로필", url: "/mypage", icon: User },
      { title: "정보수정", url: "/mypage/edit", icon: Settings },
      // { title: "활동기록", url: "/mypage/history", icon: Calendar },
      // 커뮤니티 기능 추가 시 주석 해제
    ],
  },
  {
    group: "내 투자",
    menus: [
      { title: "포트폴리오", url: "/mypage/portfolio", icon: ChartNoAxesCombined },
      { title: "거래내역", url: "/mypage/transactions", icon: ReceiptText },
      { title: "관심종목", url: "/mypage/watchlist", icon: Heart },
      { title: "지정가 주문목록", url: "/mypage/pending-orders", icon: ClipboardList },
    ],
  },
];
