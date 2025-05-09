package com.example.chickenstock.api

import android.util.Log
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import okhttp3.*
import org.json.JSONObject
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

/**
 * 웹소켓 연결을 관리하는 싱글톤 클래스
 */
class WebSocketManager private constructor() {
    companion object {
        private const val TAG = "WebSocketManager"
        private const val WS_URL = "wss://k12a106.p.ssafy.io/ws/stock"
        
        @Volatile
        private var instance: WebSocketManager? = null
        
        fun getInstance(): WebSocketManager {
            return instance ?: synchronized(this) {
                instance ?: WebSocketManager().also { instance = it }
            }
        }
    }
    
    // 현재가 상태
    private val _stockPrice = MutableStateFlow<StockPrice?>(null)
    val stockPrice: StateFlow<StockPrice?> = _stockPrice.asStateFlow()
    
    // 호가 상태
    private val _stockBidAsk = MutableStateFlow<StockBidAsk?>(null)
    val stockBidAsk: StateFlow<StockBidAsk?> = _stockBidAsk.asStateFlow()
    
    private var client: OkHttpClient? = null
    private var webSocket: WebSocket? = null
    private var currentStockCode: String? = null
    private var isConnecting = false
    private var reconnectAttempts = 0
    private val maxReconnectAttempts = 5
    private var reconnectThread: Thread? = null
    
    /**
     * 웹소켓 연결 초기화
     */
    fun connect(stockCode: String) {
        Log.d(TAG, "연결 요청: $stockCode")
        
        // 이미 같은 종목에 연결 중인 경우 무시
        if (isConnecting && stockCode == currentStockCode) {
            Log.d(TAG, "이미 연결 중: $stockCode")
            return
        }
        
        // 먼저 이전 연결을 종료
        disconnect()
        
        isConnecting = true
        currentStockCode = stockCode
        reconnectAttempts = 0
        
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
        
        webSocket = client?.newWebSocket(request, createWebSocketListener())
        
        Log.d(TAG, "웹소켓 연결 시작: $stockCode")
    }
    
    /**
     * 웹소켓 연결 해제
     */
    fun disconnect() {
        Log.d(TAG, "웹소켓 연결 해제")
        
        reconnectThread?.interrupt()
        reconnectThread = null
        
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
        
        isConnecting = false
        currentStockCode = null
        
        // 상태 초기화
        _stockPrice.value = null
        _stockBidAsk.value = null
    }
    
    /**
     * 웹소켓 리스너 생성
     */
    private fun createWebSocketListener(): WebSocketListener {
        return object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                Log.d(TAG, "웹소켓 연결 성공")
                isConnecting = true
                reconnectAttempts = 0
                
                // 구독 요청 전송
                currentStockCode?.let { stockCode ->
                    // 정확한 형식의 구독 메시지
                    val subscribeMessage = JSONObject().apply {
                        put("action", "subscribe")
                        put("stockCode", stockCode)
                    }.toString()
                    
                    webSocket.send(subscribeMessage)
                    Log.d(TAG, "구독 요청 전송: $subscribeMessage")
                }
            }
            
            override fun onMessage(webSocket: WebSocket, text: String) {
                Log.d(TAG, "메시지 수신: $text")
                
                try {
                    val json = JSONObject(text)
                    val messageType = json.optString("type", "")
                    
                    when {
                        // 연결 확인 메시지
                        messageType == "connected" -> {
                            Log.d(TAG, "웹소켓 연결 완료: ${json.optString("message", "")}")
                        }
                        
                        // 오류 메시지
                        messageType == "error" -> {
                            Log.e(TAG, "웹소켓 오류 메시지: ${json.optString("message", "")}")
                        }
                        
                        // 현재가 데이터 (여러 가능한 형식 모두 처리)
                        messageType == "stockPrice" || messageType == "price" || json.has("currentPrice") -> {
                            // 로그에 전체 메시지 내용 출력 (디버깅용)
                            Log.d(TAG, "현재가 메시지: $text")
                            
                            val stockPrice = StockPrice(
                                type = messageType.ifEmpty { "price" },
                                stockCode = json.optString("stockCode", currentStockCode ?: ""),
                                timestamp = json.optString("timestamp", ""),
                                currentPrice = json.optString("currentPrice", "0"),
                                priceChange = json.optString("priceChange", "0"),
                                changeRate = json.optString("changeRate", "0")
                            )
                            _stockPrice.value = stockPrice
                            Log.d(TAG, "현재가 데이터 처리 완료: ${stockPrice.currentPrice}, 변동: ${stockPrice.priceChange}")
                        }
                        
                        // 호가 데이터 (여러 가능한 형식 모두 처리)
                        messageType == "stockBidAsk" || messageType == "bidask" || json.has("askPrices") || json.has("bidPrices") -> {
                            // 로그에 전체 메시지 내용 출력 (디버깅용)
                            Log.d(TAG, "호가 메시지: $text")
                            
                            try {
                                // 매도 호가 및 수량
                                val askPrices = mutableMapOf<String, String>()
                                val askVolumes = mutableMapOf<String, String>()
                                val bidPrices = mutableMapOf<String, String>()
                                val bidVolumes = mutableMapOf<String, String>()
                                
                                // 매도 호가 처리
                                if (json.has("askPrices")) {
                                    val askPricesJson = json.getJSONObject("askPrices")
                                    val askVolumesJson = json.getJSONObject("askVolumes")
                                    
                                    val iterator = askPricesJson.keys()
                                    while (iterator.hasNext()) {
                                        val key = iterator.next()
                                        askPrices[key] = askPricesJson.getString(key)
                                        askVolumes[key] = askVolumesJson.getString(key)
                                    }
                                }
                                
                                // 매수 호가 처리
                                if (json.has("bidPrices")) {
                                    val bidPricesJson = json.getJSONObject("bidPrices")
                                    val bidVolumesJson = json.getJSONObject("bidVolumes")
                                    
                                    val iterator = bidPricesJson.keys()
                                    while (iterator.hasNext()) {
                                        val key = iterator.next()
                                        bidPrices[key] = bidPricesJson.getString(key)
                                        bidVolumes[key] = bidVolumesJson.getString(key)
                                    }
                                }
                                
                                val stockBidAsk = StockBidAsk(
                                    type = messageType.ifEmpty { "bidask" },
                                    stockCode = json.optString("stockCode", currentStockCode ?: ""),
                                    timestamp = json.optString("timestamp", ""),
                                    askPrices = askPrices,
                                    askVolumes = askVolumes,
                                    bidPrices = bidPrices,
                                    bidVolumes = bidVolumes
                                )
                                _stockBidAsk.value = stockBidAsk
                                Log.d(TAG, "호가 데이터 처리 완료: 매도 ${askPrices.size}건, 매수 ${bidPrices.size}건")
                            } catch (e: Exception) {
                                Log.e(TAG, "호가 데이터 처리 오류", e)
                            }
                        }
                        
                        // 기타 알 수 없는 메시지
                        else -> {
                            Log.d(TAG, "미처리 메시지 타입: ${if (messageType.isEmpty()) "없음" else messageType}, 내용: $text")
                            
                            // 가능한 형식의 데이터가 있는지 확인
                            if (text.contains("currentPrice") || text.contains("priceChange") || text.contains("changeRate")) {
                                Log.d(TAG, "현재가 관련 데이터가 포함되어 있지만 처리되지 않았습니다.")
                            }
                            if (text.contains("askPrices") || text.contains("bidPrices")) {
                                Log.d(TAG, "호가 관련 데이터가 포함되어 있지만 처리되지 않았습니다.")
                            }
                        }
                    }
                } catch (e: Exception) {
                    Log.e(TAG, "메시지 처리 오류", e)
                }
            }
            
            override fun onClosing(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "웹소켓 종료 중: $code, $reason")
                webSocket.close(1000, "정상 종료")
            }
            
            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "웹소켓 종료됨: $code, $reason")
                isConnecting = false
                
                // 정상 종료가 아닌 경우 재연결 시도
                if (code != 1000 && reconnectAttempts < maxReconnectAttempts) {
                    attemptReconnect()
                }
            }
            
            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                Log.e(TAG, "웹소켓 오류: ${t.message}", t)
                isConnecting = false
                
                // 재연결 시도
                if (reconnectAttempts < maxReconnectAttempts) {
                    attemptReconnect()
                }
            }
        }
    }
    
    /**
     * 웹소켓 재연결 시도
     */
    private fun attemptReconnect() {
        reconnectAttempts++
        val delay = 1000L * reconnectAttempts // 지수적 백오프
        
        Log.d(TAG, "재연결 시도 ${reconnectAttempts}/${maxReconnectAttempts}, ${delay}ms 후")
        
        reconnectThread = thread {
            try {
                Thread.sleep(delay)
                currentStockCode?.let { connect(it) }
            } catch (e: InterruptedException) {
                Log.d(TAG, "재연결 취소됨")
            } catch (e: Exception) {
                Log.e(TAG, "재연결 오류", e)
            }
        }
    }
} 