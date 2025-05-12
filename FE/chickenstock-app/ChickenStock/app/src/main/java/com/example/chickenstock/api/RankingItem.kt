package com.example.chickenstock.api

data class RankingItem(
    val stockCode: String,
    val stockName: String,
    val currentPrice: String,
    val fluctuationRate: String,
    val tradeVolume: String? = null,
    val contractStrength: String? = null,
    val tradeAmount: String
) 