package com.example.chickenstock.model

data class PortfolioData(
    val memberMoney: Int,
    val totalAsset: Int,
    val totalInvestment: Int,
    val totalValuation: Int,
    val totalProfitLoss: Int,
    val totalReturnRate: Double,
    val positions: List<Position>,
    val updatedAt: String,
    val cashRatio: Double
)

data class Position(
    val stockCode: String,
    val stockName: String,
    val quantity: Int,
    val averagePrice: Int,
    val currentPrice: Int,
    val valuationAmount: Int,
    val profitLoss: Int,
    val returnRate: Double,
    val allocationRatio: Double
)

data class StockUpdate(
    val type: String,
    val stockCode: String,
    val currentPrice: Int,
    val valuationAmount: Int,
    val profitLoss: Int,
    val returnRate: Double,
    val totalData: TotalData
)

data class TotalData(
    val totalAsset: Int,
    val totalProfitLoss: Int,
    val totalReturnRate: Double
)

data class TradeHistoryResponse(
    val tradeHistories: List<TradeHistory>,
    val realizedProfit: Int,
    val hasNext: Boolean,
    val nextCursor: String?
)

data class TradeHistory(
    val stockName: String,
    val tradeType: String, // "BUY" or "SELL"
    val quantity: Int,
    val unitPrice: Int,
    val createdAt: String,
    val tradedAt: String
) 