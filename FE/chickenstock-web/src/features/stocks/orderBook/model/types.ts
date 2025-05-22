export interface InitialOrderBook {
  bid_req_base_tm: string; // 호가 기준 시간 (HHMMSS)
  sel_10th_pre_req: string; // 매도 10차 잔량
  sel_10th_pre_bid: string; // 매도 10차 호가
  sel_9th_pre_req: string; // 매도 9차 잔량
  sel_9th_pre_bid: string; // 매도 9차 호가
  sel_8th_pre_req: string; // 매도 8차 잔량
  sel_8th_pre_bid: string; // 매도 8차 호가
  sel_7th_pre_req: string; // 매도 7차 잔량
  sel_7th_pre_bid: string; // 매도 7차 호가
  sel_6th_pre_req: string; // 매도 6차 잔량
  sel_6th_pre_bid: string; // 매도 6차 호가
  sel_5th_pre_req: string; // 매도 5차 잔량
  sel_5th_pre_bid: string; // 매도 5차 호가
  sel_4th_pre_req: string; // 매도 4차 잔량
  sel_4th_pre_bid: string; // 매도 4차 호가
  sel_3th_pre_req: string; // 매도 3차 잔량
  sel_3th_pre_bid: string; // 매도 3차 호가
  sel_2th_pre_req: string; // 매도 2차 잔량
  sel_2th_pre_bid: string; // 매도 2차 호가
  sel_fpr_req: string; // 매도 최우선 잔량(1차)
  sel_fpr_bid: string; // 매도 최우선 호가(1차)
  buy_fpr_bid: string; // 매수 최우선 호가(1차)
  buy_fpr_req: string; // 매수 최우선 잔량(1차)
  buy_2th_pre_bid: string; // 매수 2차 호가
  buy_2th_pre_req: string; // 매수 2차 잔량
  buy_3th_pre_bid: string; // 매수 3차 호가
  buy_3th_pre_req: string; // 매수 3차 잔량
  buy_4th_pre_bid: string; // 매수 4차 호가
  buy_4th_pre_req: string; // 매수 4차 잔량
  buy_5th_pre_bid: string; // 매수 5차 호가
  buy_5th_pre_req: string; // 매수 5차 잔량
  buy_6th_pre_bid: string; // 매수 6차 호가
  buy_6th_pre_req: string; // 매수 6차 잔량
  buy_7th_pre_bid: string; // 매수 7차 호가
  buy_7th_pre_req: string; // 매수 7차 잔량
  buy_8th_pre_bid: string; // 매수 8차 호가
  buy_8th_pre_req: string; // 매수 8차 잔량
  buy_9th_pre_bid: string; // 매수 9차 호가
  buy_9th_pre_req: string; // 매수 9차 잔량
  buy_10th_pre_bid: string; // 매수 10차 호가
  buy_10th_pre_req: string; // 매수 10차 잔량
  tot_sel_req: string; // 총 매도 잔량
  tot_buy_req: string; // 총 매수 잔량
  return_code: number;
  return_msg: string; // 응답 메시지
}

export interface RealTimeOrderBook extends OrderBookData {
  type: "stockBidAsk"; // 메시지 타입 식별자
  stockCode: string; // 종목 코드 (예: 삼성전자)
  timestamp: string; // 호가 시간 (HHMMSS 형식)
}

export interface OrderBookData {
  askPrices: {
    // 매도호가 가격
    "1": string; // 1호가 매도가격
    "2": string; // 2호가 매도가격
    "3": string; //
    "4": string; //
    "5": string; //
    "6": string; //
    "7": string; //
    "8": string; //
  };
  askVolumes: {
    // 매도호가 수량
    "1": string; // 1호가 매도수량
    "2": string; // 2호가 매도수량량
    "3": string; //
    "4": string; //
    "5": string; //
    "6": string; //
    "7": string; //
    "8": string; //
  };
  bidPrices: {
    // 매수호가 가격
    "1": string; // 1호가 매수가격
    "2": string; // 2호가 매수수가격
    "3": string; //
    "4": string; //
    "5": string; //
    "6": string; //
    "7": string; //
    "8": string; //
  };
  bidVolumes: {
    // 매수호가 수량
    "1": string; // 1호가 매수수량
    "2": string; // 2호가 매수수량
    "3": string; //
    "4": string; //
    "5": string; //
    "6": string; //
    "7": string; //
    "8": string; //
  };
}

export interface OrderBookRow {
  price: number;
  askVolume: number;
  bidVolume: number;
}

export const convertOrderBookDataToRows = (data: OrderBookData): OrderBookRow[] => {
  const rows: OrderBookRow[] = [];

  // 매도호가 (내림차순)
  for (let i = 8; i >= 1; i--) {
    const price = Math.abs(Number(data.askPrices[i.toString() as keyof typeof data.askPrices]));
    const askVolume = Number(data.askVolumes[i.toString() as keyof typeof data.askVolumes]);
    rows.push({ price, askVolume, bidVolume: 0 });
  }

  // 매수호가 (내림차순)
  for (let i = 1; i <= 8; i++) {
    const price = Math.abs(Number(data.bidPrices[i.toString() as keyof typeof data.bidPrices]));
    const bidVolume = Number(data.bidVolumes[i.toString() as keyof typeof data.bidVolumes]);
    rows.push({ price, askVolume: 0, bidVolume });
  }

  return rows;
};

export const mapInitialOrderBookToOrderBookData = (data: InitialOrderBook): OrderBookData => {
  return {
    askPrices: {
      "1": data.sel_fpr_bid,
      "2": data.sel_2th_pre_bid,
      "3": data.sel_3th_pre_bid,
      "4": data.sel_4th_pre_bid,
      "5": data.sel_5th_pre_bid,
      "6": data.sel_6th_pre_bid,
      "7": data.sel_7th_pre_bid,
      "8": data.sel_8th_pre_bid,
    },
    askVolumes: {
      "1": data.sel_fpr_req,
      "2": data.sel_2th_pre_req,
      "3": data.sel_3th_pre_req,
      "4": data.sel_4th_pre_req,
      "5": data.sel_5th_pre_req,
      "6": data.sel_6th_pre_req,
      "7": data.sel_7th_pre_req,
      "8": data.sel_8th_pre_req,
    },
    bidPrices: {
      "1": data.buy_fpr_bid,
      "2": data.buy_2th_pre_bid,
      "3": data.buy_3th_pre_bid,
      "4": data.buy_4th_pre_bid,
      "5": data.buy_5th_pre_bid,
      "6": data.buy_6th_pre_bid,
      "7": data.buy_7th_pre_bid,
      "8": data.buy_8th_pre_bid,
    },
    bidVolumes: {
      "1": data.buy_fpr_req,
      "2": data.buy_2th_pre_req,
      "3": data.buy_3th_pre_req,
      "4": data.buy_4th_pre_req,
      "5": data.buy_5th_pre_req,
      "6": data.buy_6th_pre_req,
      "7": data.buy_7th_pre_req,
      "8": data.buy_8th_pre_req,
    },
  };
};

export interface OrderBookProps {
  stockCode: string;
  currentPrice: number;
  initialData?: InitialOrderBook;
  orderBookData?: OrderBookData | null;
}

export interface OrderRowProps {
  price: number;
  askVolume: number;
  bidVolume: number;
  max: number;
  isCurrentPrice?: boolean;
}
