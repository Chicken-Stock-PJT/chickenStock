package com.example.chickenstock.model

data class StockRankingResponse(
    val rankingType: String,
    val rankingItems: List<RankingItem>,
    val hasNext: Boolean,
    val nextKey: String,
    val code: Int,
    val message: String
)

data class RankingItem(
    val stockCode: String,
    val stockName: String,
    val currentPrice: String,
    val previousDayCompareSign: String,
    val previousDayCompare: String,
    val fluctuationRate: String,
    val currentRank: String,
    val previousRank: String,
    val currentTradeVolume: String,
    val previousTradeVolume: String,
    val tradeAmount: String,
    val contractStrength: String? = null,
    val tradeVolume: String = "0"
) 