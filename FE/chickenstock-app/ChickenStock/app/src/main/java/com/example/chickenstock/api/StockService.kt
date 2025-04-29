package com.example.chickenstock.api

import com.example.chickenstock.data.Stock
import com.example.chickenstock.model.StockRankingResponse
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Query
import retrofit2.http.Path

interface StockService {
    @GET("stocks/all")
    suspend fun getAllStocks(): Response<List<Stock>>

    @GET("stock/ranking/tradeAmount")
    suspend fun getTradeAmountRanking(
        @Query("marketType") marketType: String,
        @Query("nextKey") nextKey: String? = null
    ): Response<StockRankingResponse>

    @GET("stock/ranking/fluctuationRate")
    suspend fun getFluctuationRateRanking(
        @Query("marketType") marketType: String,
        @Query("sortType") sortType: String,
        @Query("nextKey") nextKey: String? = null
    ): Response<StockRankingResponse>

    @GET("stock/ranking/volume")
    suspend fun getVolumeRanking(
        @Query("marketType") marketType: String,
        @Query("nextKey") nextKey: String? = null
    ): Response<StockRankingResponse>

    @GET("stocks/code/{code}")
    suspend fun getStockDetail(
        @Path("code") code: String
    ): Response<StockDetailResponse>
}

data class StockDetailResponse(
    val shortName: String    // 종목 이름만 필요
) 