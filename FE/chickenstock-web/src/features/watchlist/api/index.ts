import apiClient from "@/shared/api/axios";
import {
  WatchlistSuccessResponse,
  WatchlistErrorResponse,
  GetWatchlistSuccessResponse,
  GetWatchlistResult,
  WatchListResult,
} from "../model/types";
import axios, { AxiosError } from "axios";
import { ErrorResponse } from "react-router-dom";

const addWatchlist = async (stockCode: string): Promise<WatchListResult> => {
  try {
    const response = await apiClient.post<WatchlistSuccessResponse>(
      `/members/watchlist/${stockCode}`,
    );
    return response.data;
  } catch (error) {
    if (axios.isAxiosError<ErrorResponse>(error)) {
      console.log("error", error);
      return error;
    }
    throw error;
  }
};

const getWatchlist = async (): Promise<GetWatchlistResult> => {
  try {
    const response = await apiClient.get<GetWatchlistSuccessResponse>("/members/watchlist");
    console.log("response", response);
    return response.data;
  } catch (error) {
    console.log("error", error);
    if (error instanceof AxiosError) {
      return error;
    }
    throw error;
  }
};

const deleteWatchlist = async (
  stockCode: string,
): Promise<WatchlistSuccessResponse | WatchlistErrorResponse | AxiosError<ErrorResponse>> => {
  try {
    const response = await apiClient.delete<
      WatchlistSuccessResponse | WatchlistErrorResponse | AxiosError<ErrorResponse>
    >(`/members/watchlist/${stockCode}`);
    return response.data;
  } catch (error) {
    console.log("error", error);
    if (error instanceof AxiosError) {
      return error;
    }
    throw error;
  }
};

export { addWatchlist, getWatchlist, deleteWatchlist };

/*
api: get, add, delete -> get을 하면 store에 저장을 해
query; get
mutation : add, delete -> get 무효화화
*/
