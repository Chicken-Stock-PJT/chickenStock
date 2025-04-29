package com.example.chickenstock.api

import android.content.Context
import com.example.chickenstock.data.TokenManager
import kotlinx.coroutines.runBlocking
import okhttp3.Authenticator
import okhttp3.Request
import okhttp3.Response
import okhttp3.Route
import retrofit2.HttpException

class TokenAuthenticator(private val context: Context) : Authenticator {
    override fun authenticate(route: Route?, response: Response): Request? {
        val tokenManager = TokenManager.getInstance(context)
        
        // 리프레시 토큰이 없으면 null 반환
        val refreshToken = tokenManager.getRefreshToken() ?: return null
        val accessToken = tokenManager.getAccessToken() ?: return null

        synchronized(this) {
            return runBlocking {
                try {
                    // 토큰 갱신 시도
                    val authService = RetrofitClient.getInstance(context).create(AuthService::class.java)
                    val refreshResponse = authService.refreshAllTokens(
                        TokenRefreshRequest(accessToken, refreshToken)
                    )

                    if (refreshResponse.isSuccessful && refreshResponse.body() != null) {
                        val newTokens = refreshResponse.body()!!
                        // 새로운 토큰 저장
                        tokenManager.saveTokens(
                            newTokens.accessToken,
                            newTokens.refreshToken,
                            3600000L // 1시간
                        )
                        
                        // 새로운 액세스 토큰으로 요청 재시도
                        response.request.newBuilder()
                            .header("Authorization", "Bearer ${newTokens.accessToken}")
                            .build()
                    } else {
                        // 토큰 갱신 실패 시 토큰 삭제
                        tokenManager.clearTokens()
                        null
                    }
                } catch (e: Exception) {
                    // 에러 발생 시 토큰 삭제
                    tokenManager.clearTokens()
                    null
                }
            }
        }
    }
} 