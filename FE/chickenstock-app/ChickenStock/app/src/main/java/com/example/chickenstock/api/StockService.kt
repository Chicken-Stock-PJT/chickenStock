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

data class StockDetailResponse(
    val stockCode: String,          // 종목 코드
    val shortName: String,          // 종목 이름
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
    suspend fun buyStock(@Body request: BuyOrderRequest): Response<BuyOrderResponse>

    @Headers("Content-Type: application/json")
    @POST("stock/trading/sell")
    suspend fun sellStock(@Body request: SellOrderRequest): Response<SellOrderResponse>

    @GET("stocks/info/{stockCode}")
    suspend fun getStockInfo(@Path("stockCode") stockCode: String): Response<StockDetailResponse>

    @GET("stocks/askbid/{code}")
    suspend fun getStockBidAsk(@Path("code") stockCode: String): Response<StockBidAskResponse>
} 