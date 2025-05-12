package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.POST

// 토큰 교환을 위한 API 서비스 인터페이스
interface TokenExchangeService {
    @POST("auth/exchange")
    suspend fun exchangeToken(@Body request: TokenExchangeRequest): Response<LoginResponse>
}

// 토큰 교환 요청 데이터 클래스
data class TokenExchangeRequest(
    val oneTimeCode: String,
    val platform: String = "mobile"
) 