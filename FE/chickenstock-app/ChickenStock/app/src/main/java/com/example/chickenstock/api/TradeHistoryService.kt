package com.example.chickenstock.api

import retrofit2.http.GET
import retrofit2.http.Header
import retrofit2.http.Query
import com.example.chickenstock.model.TradeHistoryResponse
import retrofit2.http.Path

interface TradeHistoryService {
    @GET("trade-histories")
    suspend fun getTradeHistories(
        @Header("Authorization") token: String,
        @Query("size") size: Int,
        @Query("cursor") cursor: String? = null
    ): TradeHistoryResponse

    @GET("trade-histories/{memberId}")
    suspend fun getAiTradeHistories(
        @Path("memberId") memberId: Int,
        @Query("size") size: Int,
        @Query("cursor") cursor: String? = null
    ): com.example.chickenstock.model.TradeHistoryResponse
} 