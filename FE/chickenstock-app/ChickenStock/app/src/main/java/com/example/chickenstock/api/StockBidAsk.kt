package com.example.chickenstock.api

/**
 * 주식 호가 데이터 클래스
 */
data class StockBidAsk(
    val type: String,
    val stockCode: String,
    val timestamp: String,
    val askPrices: Map<String, String>, // 매도 호가
    val askVolumes: Map<String, String>, // 매도 수량
    val bidPrices: Map<String, String>, // 매수 호가
    val bidVolumes: Map<String, String> // 매수 수량
) 