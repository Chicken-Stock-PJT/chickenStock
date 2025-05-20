package com.example.chickenstock.api

data class StockPrice(
    val type: String = "",
    val stockCode: String = "",
    val currentPrice: String = "",
    val priceChange: String = "",
    val changeRate: String = "",
    val timestamp: String = ""
) {
    fun getCurrentPriceAsInt(): Int {
        return currentPrice.replace(",", "").toIntOrNull() ?: 0
    }

    fun isPositiveChange(): Boolean {
        return priceChange.startsWith("+") || 
               (!priceChange.startsWith("-") && priceChange.toIntOrNull() ?: 0 > 0)
    }
} 