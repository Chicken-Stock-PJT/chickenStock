import { AxiosError } from "axios";
import { ErrorResponse } from "react-router-dom";

export type GetWatchlistResult =
  | GetWatchlistSuccessResponse
  | WatchlistErrorResponse
  | AxiosError<ErrorResponse>;

export type WatchListResult =
  | WatchlistSuccessResponse
  | WatchlistErrorResponse
  | AxiosError<ErrorResponse>;

export type WatchlistSuccessResponse = null;

export interface Watchlist {
  stockCode: string;
  stockName: string;
  currentPrice: number;
  priceChange: string;
  changeRate: string;
  tradingVolume: string;
  timestamp: string;
}

export interface GetWatchlistSuccessResponse {
  message: string;
  watchList: Watchlist[];
  updatedAt: string;
}

export interface WatchlistErrorResponse {
  status: string;
  message: string;
  data: null;
}
