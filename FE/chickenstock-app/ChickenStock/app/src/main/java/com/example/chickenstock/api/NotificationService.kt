package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.POST
import retrofit2.http.DELETE
import retrofit2.http.Header

// FCM 토큰 등록/삭제 요청 바디
data class FcmTokenRequest(val token: String)

data class FcmTokenResponse(
    val success: Boolean,
    val message: String
)

interface NotificationService {
    @POST("notification/fcm/token")
    suspend fun registerFcmToken(
        @Header("Authorization") token: String,
        @Body body: FcmTokenRequest
    ): Response<FcmTokenResponse>

    @POST("notification/fcm/token/delete")
    suspend fun deleteFcmToken(
        @Header("Authorization") token: String,
        @Body body: FcmTokenRequest
    ): Response<FcmTokenResponse>
} 