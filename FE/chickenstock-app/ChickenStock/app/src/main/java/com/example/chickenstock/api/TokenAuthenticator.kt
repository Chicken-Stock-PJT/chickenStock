package com.example.chickenstock.api

import android.content.Context
import com.example.chickenstock.data.TokenManager
import kotlinx.coroutines.runBlocking
import okhttp3.Authenticator
import okhttp3.Request
import okhttp3.Response
import okhttp3.Route
import retrofit2.HttpException
import android.util.Log

class TokenAuthenticator(private val context: Context) : Authenticator {
    companion object {
        private const val TAG = "TokenAuthenticator"
        private var isRefreshing = false
    }
    
    override fun authenticate(route: Route?, response: Response): Request? {
        val tokenManager = TokenManager.getInstance(context)
        
        // 이미 토큰 갱신 중인 경우 중복 요청 방지
        if (isRefreshing) {
            Log.d(TAG, "이미 토큰 갱신 중입니다. 잠시 후 다시 시도합니다.")
            Thread.sleep(1000) // 1초 대기
            val newToken = tokenManager.getAccessToken()
            return if (newToken != null) {
                response.request.newBuilder()
                    .header("Authorization", "Bearer $newToken")
                    .build()
            } else {
                null
            }
        }
        
        // 리프레시 토큰이 없으면 null 반환
        val refreshToken = tokenManager.getRefreshToken()
        val accessToken = tokenManager.getAccessToken()
        
        if (refreshToken == null || accessToken == null) {
            Log.e(TAG, "토큰이 없습니다. 재인증이 필요합니다.")
            tokenManager.clearTokens()
            return null
        }

        synchronized(this) {
            isRefreshing = true
            try {
                return runBlocking {
                    try {
                        Log.d(TAG, "토큰 갱신 시도: ${response.request.url}")
                        // 토큰 갱신 시도
                        val authService = RetrofitClient.getInstance(context).create(AuthService::class.java)
                        val refreshResponse = authService.refreshAllTokens(
                            TokenRefreshRequest(accessToken, refreshToken)
                        )

                        if (refreshResponse.isSuccessful && refreshResponse.body() != null) {
                            val newTokens = refreshResponse.body()!!
                            Log.d(TAG, "토큰 갱신 성공")
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
                            Log.e(TAG, "토큰 갱신 실패: ${refreshResponse.code()}")
                            tokenManager.clearTokens()
                            null
                        }
                    } catch (e: Exception) {
                        // 에러 발생 시 토큰 삭제
                        Log.e(TAG, "토큰 갱신 중 오류 발생: ${e.message}", e)
                        tokenManager.clearTokens()
                        null
                    }
                }
            } finally {
                // 갱신 작업 완료 후 isRefreshing 플래그 초기화
                isRefreshing = false
                Log.d(TAG, "토큰 갱신 프로세스 완료, isRefreshing = false")
            }
        }
    }
} 