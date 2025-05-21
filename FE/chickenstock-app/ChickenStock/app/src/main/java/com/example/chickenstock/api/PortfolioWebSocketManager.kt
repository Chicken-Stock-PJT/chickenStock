package com.example.chickenstock.api

import android.content.Context
import android.util.Log
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.Position
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import okhttp3.*
import org.json.JSONObject
import java.util.concurrent.TimeUnit

/**
 * 포트폴리오 웹소켓 연결을 관리하는 싱글톤 클래스
 */
class PortfolioWebSocketManager private constructor() {
    companion object {
        private const val TAG = "PortfolioWebSocketManager"
        private const val WS_URL = "wss://chickenstock.shop/ws/portfolio"
        
        @Volatile
        private var instance: PortfolioWebSocketManager? = null
        
        fun getInstance(): PortfolioWebSocketManager {
            return instance ?: synchronized(this) {
                instance ?: PortfolioWebSocketManager().also { instance = it }
            }
        }
    }
    
    // 포트폴리오 실시간 데이터 상태
    private val _portfolioUpdateFlow = MutableStateFlow<PortfolioData?>(null)
    val portfolioUpdateFlow: StateFlow<PortfolioData?> = _portfolioUpdateFlow.asStateFlow()
    
    private var client: OkHttpClient? = null
    private var webSocket: WebSocket? = null
    private var isConnected = false
    
    /**
     * 웹소켓 연결 초기화
     */
    fun connect(context: Context) {
        // 이미 연결된 경우 무시
        if (isConnected) {
            Log.d(TAG, "이미 연결 중입니다")
            return
        }
        
        val tokenManager = TokenManager.getInstance(context)
        val token = tokenManager.getAccessToken() ?: return
        
        // OkHttpClient 초기화
        client = OkHttpClient.Builder()
            .connectTimeout(5, TimeUnit.SECONDS)
            .readTimeout(5, TimeUnit.SECONDS)
            .writeTimeout(5, TimeUnit.SECONDS)
            .retryOnConnectionFailure(true)
            .build()
        
        // 웹소켓 연결 요청
        val request = Request.Builder()
            .url(WS_URL)
            .build()
        
        webSocket = client?.newWebSocket(request, createWebSocketListener(token))
        
        Log.d(TAG, "포트폴리오 웹소켓 연결 시작")
    }
    
    /**
     * 웹소켓 연결 해제
     */
    fun disconnect() {
        Log.d(TAG, "포트폴리오 웹소켓 연결 해제")
        
        try {
            webSocket?.close(1000, "연결 종료")
            webSocket = null
        } catch (e: Exception) {
            Log.e(TAG, "웹소켓 종료 오류", e)
        }
        
        try {
            client?.dispatcher?.executorService?.shutdown()
            client = null
        } catch (e: Exception) {
            Log.e(TAG, "Client 종료 오류", e)
        }
        
        isConnected = false
        
        // 상태 초기화
        _portfolioUpdateFlow.value = null
    }
    
    /**
     * 웹소켓 리스너 생성
     */
    private fun createWebSocketListener(token: String): WebSocketListener {
        return object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                Log.d(TAG, "포트폴리오 웹소켓 연결 성공")
                isConnected = true
                
                // 인증 요청 전송
                val authenticateMessage = JSONObject().apply {
                    put("action", "authenticate")
                    put("token", "Bearer $token")
                }
                
                webSocket.send(authenticateMessage.toString())
                Log.d(TAG, "인증 요청 전송")
            }
            
            override fun onMessage(webSocket: WebSocket, text: String) {
                // 메시지가 너무 길 경우 로그 길이 제한
                val logText = if (text.length > 500) text.substring(0, 500) + "..." else text
                Log.d(TAG, "포트폴리오 메시지 수신: $logText")
                
                try {
                    val json = JSONObject(text)
                    val messageType = json.optString("type", "")
                    
                    when (messageType) {
                        // 인증 성공 메시지
                        "authenticated" -> {
                            Log.d(TAG, "포트폴리오 웹소켓 인증 성공: ${json.optString("memberId", "")}")
                        }
                        
                        // 전체 포트폴리오 업데이트 (fullPortfolioUpdate)
                        "fullPortfolioUpdate" -> {
                            Log.d(TAG, "전체 포트폴리오 업데이트 수신")
                            
                            val dataJson = json.optJSONObject("data")
                            if (dataJson != null) {
                                val memberMoney = dataJson.optInt("memberMoney")
                                val totalAsset = dataJson.optInt("totalAsset")
                                val totalInvestment = dataJson.optInt("totalInvestment")
                                val totalValuation = dataJson.optInt("totalValuation") 
                                val totalProfitLoss = dataJson.optInt("totalProfitLoss")
                                val totalReturnRate = dataJson.optDouble("totalReturnRate")
                                val updatedAt = dataJson.optString("updatedAt")
                                
                                // 포지션 정보 처리
                                val positionsArray = dataJson.optJSONArray("positions")
                                val positions = mutableListOf<Position>()
                                
                                if (positionsArray != null) {
                                    for (i in 0 until positionsArray.length()) {
                                        val positionJson = positionsArray.getJSONObject(i)
                                        
                                        // 포지션 객체 생성
                                        val position = Position(
                                            stockCode = positionJson.optString("stockCode"),
                                            stockName = positionJson.optString("stockName"),
                                            quantity = positionJson.optInt("quantity"),
                                            averagePrice = positionJson.optInt("averagePrice"),
                                            currentPrice = positionJson.optInt("currentPrice"),
                                            valuationAmount = positionJson.optInt("valuationAmount"),
                                            profitLoss = positionJson.optInt("profitLoss"),
                                            returnRate = positionJson.optDouble("returnRate"),
                                            // allocationRatio는 직접 계산
                                            allocationRatio = if (totalAsset > 0) positionJson.optInt("valuationAmount").toDouble() / totalAsset.toDouble() else 0.0
                                        )
                                        
                                        positions.add(position)
                                    }
                                }
                                
                                // 캐시 비율 계산
                                val cashRatio = if (totalAsset > 0) memberMoney.toDouble() / totalAsset.toDouble() else 1.0
                                
                                // 새로운 PortfolioData 객체 생성
                                val updatedPortfolioData = PortfolioData(
                                    memberMoney = memberMoney,
                                    totalAsset = totalAsset,
                                    totalInvestment = totalInvestment,
                                    totalValuation = totalValuation,
                                    totalProfitLoss = totalProfitLoss,
                                    totalReturnRate = totalReturnRate,
                                    positions = positions,
                                    updatedAt = updatedAt,
                                    cashRatio = cashRatio
                                )
                                
                                // StateFlow 업데이트
                                _portfolioUpdateFlow.value = updatedPortfolioData
                                Log.d(TAG, "전체 포트폴리오 데이터 업데이트 완료: ${positions.size}개 종목, 총자산: ${totalAsset}, 수익률: ${totalReturnRate}%")
                            }
                        }
                        
                        // 실시간 주식 업데이트 (stockupdate)
                        "stockupdate" -> {
                            Log.d(TAG, "실시간 포트폴리오 업데이트 수신")
                            
                            // 현재 상태 복사
                            val currentData = _portfolioUpdateFlow.value ?: return
                            
                            // 업데이트할 종목 정보
                            val stockCode = json.optString("stockCode")
                            val currentPrice = json.optInt("currentPrice")
                            val valuationAmount = json.optInt("valuationAmount")
                            val profitLoss = json.optInt("profitLoss")
                            val returnRate = json.optDouble("returnRate")
                            
                            // totalData 정보
                            val totalDataJson = json.optJSONObject("totalData")
                            if (totalDataJson != null) {
                                val totalAsset = totalDataJson.optInt("totalAsset")
                                val totalProfitLoss = totalDataJson.optInt("totalProfitLoss")
                                val totalReturnRate = totalDataJson.optDouble("totalReturnRate")
                                
                                // Position 목록에서 해당 종목 찾아 업데이트
                                val updatedPositions = currentData.positions.map { position ->
                                    if (position.stockCode == stockCode) {
                                        position.copy(
                                            currentPrice = currentPrice,
                                            valuationAmount = valuationAmount,
                                            profitLoss = profitLoss,
                                            returnRate = returnRate
                                        )
                                    } else {
                                        position
                                    }
                                }
                                
                                // 전체 데이터 업데이트
                                val updatedPortfolioData = currentData.copy(
                                    totalAsset = totalAsset,
                                    totalValuation = updatedPositions.sumOf { it.valuationAmount },
                                    totalProfitLoss = totalProfitLoss,
                                    totalReturnRate = totalReturnRate,
                                    positions = updatedPositions,
                                    updatedAt = java.time.LocalDateTime.now().toString()  // 업데이트 시간 갱신
                                )
                                
                                _portfolioUpdateFlow.value = updatedPortfolioData
                                Log.d(TAG, """
                                    포트폴리오 데이터 업데이트 완료:
                                    - 종목: $stockCode
                                    - 현재가: $currentPrice
                                    - 평가금액: $valuationAmount
                                    - 수익: $profitLoss
                                    - 수익률: $returnRate
                                    - 총자산: $totalAsset
                                    - 총수익: $totalProfitLoss
                                    - 총수익률: $totalReturnRate
                                """.trimIndent())
                            }
                        }
                        
                        // 에러 메시지
                        "error" -> {
                            Log.e(TAG, "포트폴리오 웹소켓 오류: ${json.optString("message", "")}")
                        }
                    }
                } catch (e: Exception) {
                    Log.e(TAG, "포트폴리오 메시지 처리 오류", e)
                }
            }
            
            override fun onClosing(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "포트폴리오 웹소켓 종료 중: $code, $reason")
                webSocket.close(1000, "정상 종료")
            }
            
            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "포트폴리오 웹소켓 종료됨: $code, $reason")
                isConnected = false
            }
            
            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                Log.e(TAG, "포트폴리오 웹소켓 오류: ${t.message}", t)
                isConnected = false
            }
        }
    }
    
    /**
     * 최신 포트폴리오 데이터 설정
     */
    fun setInitialPortfolioData(portfolioData: PortfolioData) {
        _portfolioUpdateFlow.value = portfolioData
    }
} 