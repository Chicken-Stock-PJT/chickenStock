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

data class Order(
    val orderId: Int,
    val stockCode: String,
    val stockName: String,
    val orderType: String, // "BUY" or "SELL"
    val quantity: Int,
    val targetPrice: Int,
    val createdAt: String,
    val status: String // "PENDING"
)

data class DashboardResponse(
    val memberMoney: Int,        // 사용자 보유 현금
    val stockValuation: Int,     // 보유 주식의 현재 평가 금액
    val pendingOrderAmount: Int, // 미체결 매수 주문 대기 금액
    val totalAsset: Int,         // 총 자산
    val totalInvestment: Int,    // 총 투자 금액
    val totalProfitLoss: Int,    // 총 손익
    val totalReturnRate: Double, // 총 수익률
    val todayProfitLoss: Int,    // 오늘 실현 손익
    val todayReturnRate: Double, // 오늘 수익률
    val holdingStockCount: Int,  // 보유 종목 수
    val holdings: List<DashboardHolding>,
    val updatedAt: String?       // 대시보드 데이터 업데이트 시간 (nullable)
)

data class DashboardHolding(
    val stockCode: String,
    val stockName: String,
    val quantity: Int,
    val averagePrice: Int,
    val currentPrice: Int,
    val valuationAmount: Int,
    val profitLoss: Int,
    val returnRate: Double,
    val priceChange: String,
    val changeRate: String
) 