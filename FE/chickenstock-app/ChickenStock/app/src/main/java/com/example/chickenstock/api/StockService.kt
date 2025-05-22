package com.example.chickenstock.api

import com.example.chickenstock.data.Stock
import com.example.chickenstock.model.StockRankingResponse
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Query
import retrofit2.http.Path
import retrofit2.http.POST
import retrofit2.http.Body
import retrofit2.http.Headers
import retrofit2.http.Header
import com.example.chickenstock.model.Order
import retrofit2.http.DELETE
import retrofit2.http.PUT

data class StockDetailResponse(
    val stockCode: String,          // 종목 코드
    val shortName: String,          // 종목 이름
    val stockName: String?,         // 종목 이름(서버에서 내려주는 경우, null 허용)
    val currentPrice: String,       // 현재가
    val changeRate: String,         // 등락률
    val priceChange: String,        // 전일대비 변동 금액
    val market: String,             // 상장된 시장
    val stockType: String,          // 주식 유형
    val faceValue: String,          // 액면가
    val return_code: Int,           // 응답 코드
    val return_msg: String          // 응답 메시지
)

data class ChartResponse(
    val stockCode: String,
    val chartType: String,
    val chartData: List<ChartData>,
    val hasNext: Boolean,
    val nextKey: String,
    val code: Int,
    val message: String
)

data class ChartData(
    val date: String,
    val currentPrice: String,
    val openPrice: String,
    val highPrice: String,
    val lowPrice: String,
    val volume: String,
    val tradingValue: String,
    val modifiedRatio: String,
    val previousClosePrice: String
)

data class BuyOrderRequest(
    val stockCode: String,
    val quantity: Int,
    val price: Long? = null,
    val marketOrder: Boolean = true
)

data class SellOrderRequest(
    val stockCode: String,
    val quantity: Int,
    val price: Long? = null,
    val marketOrder: Boolean = true
)

data class BuyOrderResponse(
    val tradeHistoryId: Int,
    val stockCode: String,
    val stockName: String,
    val tradeType: String,
    val quantity: Int,
    val unitPrice: Long,
    val totalPrice: Long,
    val tradeAt: String,
    val status: String
)

data class SellOrderResponse(
    val tradeHistoryId: Int,
    val stockCode: String,
    val stockName: String,
    val tradeType: String,
    val quantity: Int,
    val unitPrice: Long,
    val totalPrice: Long,
    val tradeAt: String,
    val status: String
)

data class CancelOrderResponse(
    val status: String,
    val message: String
)

data class RankingUser(
    val rank: Int,
    val nickname: String,
    val totalAsset: Double,
    val memberId: Int
)

data class TotalAssetRankingResponse(
    val topRankings: List<RankingUser>,
    val myRank: RankingUser? = null
)

data class CommentResponse(
    val id: Int,
    val content: String?,
    val nickname: String,
    val createdAt: String,
    val updatedAt: String,
    val children: List<CommentResponse>,
    val likeCount: Int,
    val likedByMe: Boolean?,
    val deleted: Boolean
)

data class CommentsListResponse(
    val comments: List<CommentResponse>,
    val nextCursor: String?
)

data class CommentPostRequest(
    val content: String
)

data class CommentEditRequest(
    val content: String
)

data class ReplyPostRequest(
    val content: String,
    val parentId: Int
)

data class LikeToggleResponse(
    val likeCount: Int,
    val liked: Boolean
)

data class ReturnRateRankingUser(
    val rank: Int,
    val nickname: String,
    val returnRate: Double,
    val memberId: Int
)

data class ReturnRateRankingResponse(
    val topRankings: List<ReturnRateRankingUser>,
    val myRank: ReturnRateRankingUser? = null
)

interface StockService {
    @GET("stocks/all")
    suspend fun getAllStocks(): Response<List<Stock>>

    @GET("stock/ranking/tradeAmount")
    suspend fun getTradeAmountRanking(
        @Query("marketType") marketType: String
    ): Response<StockRankingResponse>

    @GET("stock/ranking/fluctuationRate")
    suspend fun getFluctuationRateRanking(
        @Query("marketType") marketType: String,
        @Query("sortType") sortType: String
    ): Response<StockRankingResponse>

    @GET("stock/ranking/volume")
    suspend fun getVolumeRanking(
        @Query("marketType") marketType: String
    ): Response<StockRankingResponse>

    @GET("stocks/code/{code}")
    suspend fun getStockDetail(
        @Path("code") code: String
    ): Response<StockDetailResponse>

    @GET("stock/chart/{chartType}/{stockCode}")
    suspend fun getStockChart(
        @Path("chartType") chartType: String,
        @Path("stockCode") stockCode: String,
        @Query("baseDate") baseDate: String? = null,
        @Query("timeInterval") timeInterval: String? = null,
        @Query("nextKey") nextKey: String? = null,
        @Query("contYn") contYn: String? = null
    ): ChartResponse

    @GET("stock/chart/all/{stockCode}")
    suspend fun getStockChartAll(
        @Path("stockCode") stockCode: String,
        @Query("chartType") chartType: String,
        @Query("timeInterval") timeInterval: String? = null
    ): ChartResponse

    @Headers("Content-Type: application/json")
    @POST("stock/trading/buy")
    suspend fun buyStock(
        @Header("Authorization") token: String,
        @Body request: BuyOrderRequest
    ): Response<BuyOrderResponse>

    @Headers("Content-Type: application/json")
    @POST("stock/trading/sell")
    suspend fun sellStock(
        @Header("Authorization") token: String,
        @Body request: SellOrderRequest
    ): Response<SellOrderResponse>

    @GET("stocks/info/{stockCode}")
    suspend fun getStockInfo(@Path("stockCode") stockCode: String): Response<StockDetailResponse>

    @GET("stocks/askbid/{code}")
    suspend fun getStockBidAsk(@Path("code") stockCode: String): Response<StockBidAskResponse>

    @GET("stock/trading/pending-orders")
    suspend fun getPendingOrders(@Header("Authorization") token: String): Response<List<Order>>

    @POST("stock/trading/cancel-order/{orderId}")
    suspend fun cancelOrder(
        @Header("Authorization") token: String,
        @Path("orderId") orderId: Int
    ): Response<CancelOrderResponse>

    @GET("stocks/{shortCode}/comments")
    suspend fun getComments(
        @Path("shortCode") shortCode: String,
        @Query("limit") limit: Int? = null,
        @Query("cursor") cursor: String? = null
    ): Response<CommentsListResponse>

    @GET("stocks/{shortCode}/comments")
    suspend fun getCommentsWithAuth(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Query("limit") limit: Int? = null,
        @Query("cursor") cursor: String? = null
    ): Response<CommentsListResponse>

    @POST("stocks/{shortCode}/comments")
    suspend fun postComment(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Body request: CommentPostRequest
    ): Response<CommentResponse>

    @DELETE("stocks/{shortCode}/comments/{commentId}")
    suspend fun deleteComment(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Path("commentId") commentId: Int
    ): Response<Unit>

    @PUT("stocks/{shortCode}/comments/{commentId}")
    suspend fun editComment(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Path("commentId") commentId: Int,
        @Body request: CommentEditRequest
    ): Response<CommentResponse>

    @POST("stocks/{shortCode}/comments/reply")
    suspend fun postReply(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Body request: ReplyPostRequest
    ): Response<CommentResponse>

    @POST("stocks/{shortCode}/comments/{commentId}/like")
    suspend fun toggleLike(
        @Header("Authorization") token: String,
        @Path("shortCode") shortCode: String,
        @Path("commentId") commentId: Int
    ): Response<LikeToggleResponse>
}

interface RankingService {
    @GET("ranking/total-asset")
    suspend fun getTotalAssetRanking(
        @Header("Authorization") token: String? = null
    ): Response<TotalAssetRankingResponse>

    @GET("ranking/total-asset/ai")
    suspend fun getAiTotalAssetRanking(): Response<List<RankingUser>>

    @GET("ranking/return-rate")
    suspend fun getReturnRateRanking(
        @Header("Authorization") token: String? = null
    ): Response<ReturnRateRankingResponse>

    @GET("ranking/return-rate/ai")
    suspend fun getAiReturnRateRanking(): Response<List<ReturnRateRankingUser>>
} 