package com.example.chickenstock.api

import android.util.Log
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import okhttp3.*
import org.json.JSONObject
import com.example.chickenstock.model.StockUpdate
import com.example.chickenstock.model.TotalData

class PortfolioWebSocket(private val token: String) {
    private var webSocket: WebSocket? = null
    private val client = OkHttpClient()
    
    private val _stockUpdateFlow = MutableStateFlow<StockUpdate?>(null)
    val stockUpdateFlow: StateFlow<StockUpdate?> = _stockUpdateFlow

    fun connect() {
        val request = Request.Builder()
            .url("wss://your-api-domain/ws/portfolio")
            .build()

        webSocket = client.newWebSocket(request, object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) {
                // 연결 후 인증 메시지 전송
                val authMessage = JSONObject().apply {
                    put("action", "authenticate")
                    put("token", "Bearer $token")
                }
                webSocket.send(authMessage.toString())
            }

            override fun onMessage(webSocket: WebSocket, text: String) {
                try {
                    val json = JSONObject(text)
                    when (json.getString("type")) {
                        "authenticated" -> {
                            Log.d("WebSocket", "인증 성공")
                        }
                        "stockUpdate" -> {
                            val update = StockUpdate(
                                type = json.getString("type"),
                                stockCode = json.getString("stockCode"),
                                currentPrice = json.getInt("currentPrice"),
                                valuationAmount = json.getInt("valuationAmount"),
                                profitLoss = json.getInt("profitLoss"),
                                returnRate = json.getDouble("returnRate"),
                                totalData = json.getJSONObject("totalData").let { totalData ->
                                    TotalData(
                                        totalAsset = totalData.getInt("totalAsset"),
                                        totalProfitLoss = totalData.getInt("totalProfitLoss"),
                                        totalReturnRate = totalData.getDouble("totalReturnRate")
                                    )
                                }
                            )
                            _stockUpdateFlow.value = update
                        }
                        "error" -> {
                            Log.e("WebSocket", "에러: ${json.getString("message")}")
                        }
                    }
                } catch (e: Exception) {
                    Log.e("WebSocket", "메시지 파싱 에러", e)
                }
            }

            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                Log.e("WebSocket", "연결 실패", t)
            }

            override fun onClosing(webSocket: WebSocket, code: Int, reason: String) {
                Log.d("WebSocket", "연결 종료 중: $reason")
            }

            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                Log.d("WebSocket", "연결 종료됨: $reason")
            }
        })
    }

    fun disconnect() {
        webSocket?.close(1000, "정상 종료")
        webSocket = null
    }
} 