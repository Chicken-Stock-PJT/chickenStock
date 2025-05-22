package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.*
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.DashboardResponse

data class SimpleProfileResponse(
    val nickname: String,
    val totalAsset: String?,
    val returnRate: String?,
    val isOauth: String,
    val memberMoney: String?
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
    suspend fun getSimpleProfile(@Header("Authorization") token: String): Response<SimpleProfileResponse>

    @GET("members/portfolio")
    suspend fun getPortfolio(@Header("Authorization") token: String): Response<PortfolioData>

    @GET("members/watchlist")
    suspend fun getWatchlist(@Header("Authorization") token: String): Response<WatchlistResponse>

    @POST("members/watchlist/{stockCode}")
    suspend fun addToWatchlist(@Header("Authorization") token: String, @Path("stockCode") stockCode: String): Response<Unit>

    @DELETE("members/watchlist/{stockCode}")
    suspend fun removeFromWatchlist(@Header("Authorization") token: String, @Path("stockCode") stockCode: String): Response<Unit>

    @GET("members/dashboard")
    suspend fun getDashboard(@Header("Authorization") token: String): Response<DashboardResponse>

    @GET("members/{memberId}/dashboard")
    suspend fun getAIDashboard(@Path("memberId") memberId: Int): Response<com.example.chickenstock.model.DashboardResponse>
} 