package com.example.chickenstock.model

data class PortfolioData(
    val memberMoney: Int,
    val totalAsset: Int,
    val totalInvestment: Int,
    val totalValuation: Int,
    val totalProfitLoss: Int,
    val totalReturnRate: Double,
    val positions: List<Position>
)

data class Position(
    val stockCode: String,
    val stockName: String,
    val quantity: Int,
    val averagePrice: Int,
    val currentPrice: Int,
    val valuationAmount: Int,
    val profitLoss: Int,
    val returnRate: Double
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