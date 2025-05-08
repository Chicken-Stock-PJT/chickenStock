package com.example.chickenstock.data

data class ChartResponse(
    val stockCode: String,
    val chartType: String,
    val chartData: List<ChartDataPoint>,
    val hasNext: Boolean,
    val nextKey: String,
    val code: Int,
    val message: String
)

data class ChartDataPoint(
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