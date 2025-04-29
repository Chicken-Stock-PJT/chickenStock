package com.example.chickenstock.api

import android.util.Log
import com.google.gson.Gson
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import okhttp3.*
import org.json.JSONObject

class WebSocketManager private constructor() {
    private var webSocket: WebSocket? = null
    private val gson = Gson()

    private val _stockPrice = MutableStateFlow<StockPrice?>(null)
    val stockPrice: StateFlow<StockPrice?> = _stockPrice

    private val _stockBidAsk = MutableStateFlow<StockBidAsk?>(null)
    val stockBidAsk: StateFlow<StockBidAsk?> = _stockBidAsk

    fun connect(stockCode: String) {
        Log.d(TAG, "웹소켓 연결 시도: stockCode = $stockCode")
        
        val client = OkHttpClient.Builder()
            .build()

        val request = Request.Builder()
            .url("wss://k12a106.p.ssafy.io/api/ws/stock")
            .build()

        Log.d(TAG, "웹소켓 요청 URL: ${request.url}")

        webSocket = client.newWebSocket(request, object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                Log.d(TAG, "웹소켓 연결됨 - HTTP ${response.code}")
                Log.d(TAG, "응답 헤더: ${response.headers}")
                subscribe(stockCode)
            }

            override fun onMessage(webSocket: WebSocket, text: String) {
                Log.d(TAG, "메시지 수신: $text")
                try {
                    val json = JSONObject(text)
                    when (json.getString("type")) {
                        "connected" -> {
                            Log.d(TAG, "연결 성공: ${json.getString("message")}")
                        }
                        "success" -> {
                            Log.d(TAG, "구독 성공: ${json.getString("stockCode")}")
                        }
                        "stockPrice" -> {
                            Log.d(TAG, "주식 가격 데이터 수신")
                            val data = gson.fromJson(text, StockPrice::class.java)
                            _stockPrice.value = data
                        }
                        "stockBidAsk" -> {
                            Log.d(TAG, "호가 데이터 수신")
                            val data = gson.fromJson(text, StockBidAsk::class.java)
                            _stockBidAsk.value = data
                        }
                    }
                } catch (e: Exception) {
                    Log.e(TAG, "메시지 파싱 오류", e)
                }
            }

            override fun onClosing(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "연결 종료 중: code=$code, reason=$reason")
            }

            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "연결 종료됨: code=$code, reason=$reason")
            }

            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                Log.e(TAG, "웹소켓 오류 발생", t)
                Log.e(TAG, "응답 코드: ${response?.code}")
                Log.e(TAG, "응답 메시지: ${response?.message}")
                Log.e(TAG, "응답 헤더: ${response?.headers}")
            }
        })
    }

    fun subscribe(stockCode: String) {
        val message = JSONObject().apply {
            put("action", "subscribe")
            put("stockCode", stockCode)
        }.toString()
        Log.d(TAG, "구독 요청 전송: $message")
        webSocket?.send(message)
    }

    fun unsubscribe(stockCode: String) {
        val message = JSONObject().apply {
            put("action", "unsubscribe")
            put("stockCode", stockCode)
        }.toString()
        Log.d(TAG, "구독 해제 요청 전송: $message")
        webSocket?.send(message)
    }

    fun disconnect() {
        Log.d(TAG, "웹소켓 연결 해제 요청")
        webSocket?.close(1000, "연결 종료")
        webSocket = null
        _stockPrice.value = null
        _stockBidAsk.value = null
    }

    companion object {
        private const val TAG = "WebSocketManager"
        @Volatile
        private var instance: WebSocketManager? = null

        fun getInstance(): WebSocketManager {
            return instance ?: synchronized(this) {
                instance ?: WebSocketManager().also { instance = it }
            }
        }
    }
}

data class StockPrice(
    val type: String = "",
    val stockCode: String = "",
    val currentPrice: String = "",
    val priceChange: String = "",
    val changeRate: String = "",
    val timestamp: String = ""
)

data class StockBidAsk(
    val type: String = "",
    val stockCode: String = "",
    val timestamp: String = "",
    val askPrices: Map<String, String> = emptyMap(),
    val askVolumes: Map<String, String> = emptyMap(),
    val bidPrices: Map<String, String> = emptyMap(),
    val bidVolumes: Map<String, String> = emptyMap()
) 