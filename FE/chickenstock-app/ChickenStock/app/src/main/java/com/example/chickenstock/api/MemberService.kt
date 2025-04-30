package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.*
import com.example.chickenstock.model.PortfolioData

data class SimpleProfileResponse(
    val nickname: String,
    val memberMoney: String,
    val returnRate: String,
    val isOauth: String
)

data class WatchlistResponse(
    val message: String,
    val watchList: List<WatchlistItem>
)

data class WatchlistItem(
    val stockCode: String,
    val stockName: String,
    val currentPrice: Int,
    val priceChange: String,
    val changeRate: String,
    val tradingVolume: String,
    val timestamp: String
)

interface MemberService {
    @GET("members/simple-profile")
    suspend fun getSimpleProfile(): Response<SimpleProfileResponse>

    @GET("members/portfolio")
    suspend fun getPortfolio(): Response<PortfolioData>

    @GET("members/watchlist")
    suspend fun getWatchlist(): Response<WatchlistResponse>

    @POST("members/watchlist/{stockCode}")
    suspend fun addToWatchlist(@Path("stockCode") stockCode: String): Response<Unit>

    @DELETE("members/watchlist/{stockCode}")
    suspend fun removeFromWatchlist(@Path("stockCode") stockCode: String): Response<Unit>
} 