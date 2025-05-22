package com.example.chickenstock.data

data class Stock(
    val shortCode: String,
    val shortName: String,
    val market: String,
    val stockType: String,
    val faceValue: String
)

// 싱글톤으로 주식 데이터를 관리
object StockRepository {
    private var stocks: List<Stock> = emptyList()
    private var isInitialized = false

    fun setStocks(newStocks: List<Stock>) {
        stocks = newStocks
        isInitialized = true
    }

    fun getStocks(): List<Stock> = stocks

    fun isInitialized(): Boolean = isInitialized

    // 검색어로 주식 필터링 (자동완성용)
    fun searchStocks(query: String): List<Stock> {
        if (query.isEmpty()) return emptyList()
        return stocks.filter { stock ->
            stock.shortName.contains(query, ignoreCase = true) ||
            stock.shortCode.contains(query, ignoreCase = true)
        }
    }
} 