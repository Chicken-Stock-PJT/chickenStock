import { http, HttpResponse } from "msw";
import {
  chartMockData,
  fluctuationRankingMockData,
  tradeRankingMockData,
  volumeRankingMockData,
} from "./data";

export const handlers = [
  http.get("/api/stock/ranking/tradeAmount", () => {
    return HttpResponse.json(tradeRankingMockData);
  }),
  http.get("/api/stock/ranking/fluctuationRate", () => {
    return HttpResponse.json(fluctuationRankingMockData);
  }),
  http.get("/api/stock/ranking/volume", () => {
    return HttpResponse.json(volumeRankingMockData);
  }),
  http.get("/api/stock/chart/all/:stockCode", () => {
    return HttpResponse.json(chartMockData);
  }),
];
