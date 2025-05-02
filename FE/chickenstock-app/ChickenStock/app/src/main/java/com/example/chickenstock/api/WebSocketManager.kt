package com.example.chickenstock.api

import android.util.Log
import com.google.gson.Gson
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import okhttp3.*
import org.json.JSONObject

class WebSocketManager private constructor() {
    private var webSocket: WebSocket? = null
    private val client = OkHttpClient()
    private val gson = Gson()

    private val _stockPrice = MutableStateFlow<StockPrice?>(null)
    val stockPrice: StateFlow<StockPrice?> = _stockPrice

    private val _stockBidAsk = MutableStateFlow<StockBidAsk?>(null)
    val stockBidAsk: StateFlow<StockBidAsk?> = _stockBidAsk

    companion object {
        private const val TAG = "WebSocketManager"
        private const val WEBSOCKET_URL = "wss://k12a106.p.ssafy.io/ws/stock"
        
        @Volatile
        private var instance: WebSocketManager? = null

        fun getInstance(): WebSocketManager {
            return instance ?: synchronized(this) {
                instance ?: WebSocketManager().also { instance = it }
            }
        }
    }

    fun connect(stockCode: String) {
        Log.d(TAG, "웹소켓 연결 시도: $WEBSOCKET_URL")
        
        // 기존 연결이 있다면 먼저 해제
        disconnect()
        
        val request = Request.Builder()
            .url(WEBSOCKET_URL)
            .build()

        webSocket = client.newWebSocket(request, object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                Log.d(TAG, "WebSocket 연결 성공")
                // 구독 메시지 전송
                val subscribeMessage = JSONObject().apply {
                    put("action", "subscribe")
                    put("stockCode", stockCode)
                }.toString()
                webSocket.send(subscribeMessage)
                Log.d(TAG, "구독 메시지 전송: $subscribeMessage")
            }

            override fun onMessage(webSocket: WebSocket, text: String) {
                Log.d(TAG, "메시지 수신: $text")
                try {
                    val jsonObject = JSONObject(text)
                    val type = jsonObject.optString("type", "")
                    
                    when (type) {
                        "price" -> {
                            val stockPrice = gson.fromJson(text, StockPrice::class.java)
                            _stockPrice.value = stockPrice
                        }
                        "bidask" -> {
                            val stockBidAsk = gson.fromJson(text, StockBidAsk::class.java)
                            _stockBidAsk.value = stockBidAsk
                        }
                    }
                } catch (e: Exception) {
                    Log.e(TAG, "메시지 파싱 실패", e)
                }
            }

            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                Log.e(TAG, "WebSocket 연결 실패", t)
                _stockPrice.value = null
                _stockBidAsk.value = null
            }

            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                Log.d(TAG, "WebSocket 연결 종료: $reason")
                _stockPrice.value = null
                _stockBidAsk.value = null
            }
        })
    }

    fun disconnect() {
        webSocket?.let { ws ->
            try {
                ws.close(1000, "정상 종료")
            } catch (e: Exception) {
                Log.e(TAG, "WebSocket 종료 중 오류 발생", e)
            } finally {
                webSocket = null
                _stockPrice.value = null
                _stockBidAsk.value = null
            }
        }
    }

    fun sendCloseMessage(stockCode: String) {
        webSocket?.let { ws ->
            try {
                val closeMessage = JSONObject().apply {
                    put("action", "unsubscribe")
                    put("stockCode", stockCode)
                }.toString()
                ws.send(closeMessage)
                Log.d(TAG, "구독 해제 메시지 전송: $closeMessage")
            } catch (e: Exception) {
                Log.e(TAG, "구독 해제 메시지 전송 실패", e)
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
) {
    fun getCurrentPriceAsInt(): Int {
        return currentPrice.replace(",", "").toIntOrNull() ?: 0
    }

    fun isPositiveChange(): Boolean {
        return priceChange.startsWith("+") || (!priceChange.startsWith("-") && priceChange.toFloatOrNull() ?: 0f > 0f)
    }
}

data class StockBidAsk(
    val type: String = "",
    val stockCode: String = "",
    val timestamp: String = "",
    val askPrices: Map<String, String> = emptyMap(),
    val askVolumes: Map<String, String> = emptyMap(),
    val bidPrices: Map<String, String> = emptyMap(),
    val bidVolumes: Map<String, String> = emptyMap()
) 