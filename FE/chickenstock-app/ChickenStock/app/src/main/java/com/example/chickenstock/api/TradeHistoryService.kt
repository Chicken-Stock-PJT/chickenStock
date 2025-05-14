package com.example.chickenstock.api

import retrofit2.http.GET
import retrofit2.http.Header
import retrofit2.http.Query
import com.example.chickenstock.model.TradeHistoryResponse

interface TradeHistoryService {
    @GET("trade-histories")
    suspend fun getTradeHistories(
        @Header("Authorization") token: String,
        @Query("size") size: Int,
        @Query("cursor") cursor: String? = null
    ): TradeHistoryResponse
} 