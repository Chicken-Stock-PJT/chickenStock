package com.example.chickenstock.data

import android.content.Context
import android.content.SharedPreferences
import android.util.Log

class TokenManager private constructor(context: Context) {
    private val prefs: SharedPreferences = context.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE)

    companion object {
        private const val TAG = "TokenManager"
        internal const val PREF_NAME = "TokenPrefs"
        private const val KEY_ACCESS_TOKEN = "access_token"
        private const val KEY_REFRESH_TOKEN = "refresh_token"
        private const val KEY_ACCESS_TOKEN_EXPIRES_IN = "access_token_expires_in"
        private const val KEY_REFRESH_TOKEN_EXPIRES_IN = "refresh_token_expires_in"
        
        // 토큰 만료 전 갱신 시간 (10분)
        private const val REFRESH_BEFORE_EXPIRY = 10 * 60 * 1000L
        
        @Volatile
        private var instance: TokenManager? = null

        fun getInstance(context: Context): TokenManager {
            return instance ?: synchronized(this) {
                instance ?: TokenManager(context).also { instance = it }
            }
        }
    }

    fun saveTokens(accessToken: String, refreshToken: String, accessTokenExpiresIn: Long, refreshTokenExpiresIn: Long = 30 * 24 * 60 * 60 * 1000L) {
        val accessExpirationTime = System.currentTimeMillis() + accessTokenExpiresIn
        val refreshExpirationTime = System.currentTimeMillis() + refreshTokenExpiresIn
        prefs.edit().apply {
            putString(KEY_ACCESS_TOKEN, accessToken)
            putString(KEY_REFRESH_TOKEN, refreshToken)
            putLong(KEY_ACCESS_TOKEN_EXPIRES_IN, accessExpirationTime)
            putLong(KEY_REFRESH_TOKEN_EXPIRES_IN, refreshExpirationTime)
            apply()
        }
        Log.d(TAG, "토큰 저장 완료, 액세스 토큰 만료: ${accessExpirationTime - System.currentTimeMillis()}ms 후, 리프레시 토큰 만료: ${refreshExpirationTime - System.currentTimeMillis()}ms 후")
    }

    fun getAccessToken(): String? {
        val token = prefs.getString(KEY_ACCESS_TOKEN, null)
        if (token == null) {
            Log.d(TAG, "액세스 토큰이 없습니다.")
        }
        return token
    }
    
    fun getRefreshToken(): String? {
        val token = prefs.getString(KEY_REFRESH_TOKEN, null)
        if (token == null) {
            Log.d(TAG, "리프레시 토큰이 없습니다.")
        }
        return token
    }
    
    fun getAccessTokenExpiresIn(): Long = prefs.getLong(KEY_ACCESS_TOKEN_EXPIRES_IN, 0)

    fun getRefreshTokenExpiresIn(): Long = prefs.getLong(KEY_REFRESH_TOKEN_EXPIRES_IN, 0)

    fun clearTokens() {
        Log.d(TAG, "모든 토큰을 삭제합니다.")
        prefs.edit().clear().apply()
    }

    fun isTokenExpired(): Boolean {
        val expiresIn = getAccessTokenExpiresIn()
        val now = System.currentTimeMillis()
        val isExpired = now >= expiresIn
        val remainingTime = expiresIn - now
        
        Log.d(TAG, "토큰 만료 확인: ${if (isExpired) "만료됨" else "유효함"}, 남은 시간: ${remainingTime}ms")
        return isExpired
    }
    
    fun shouldRefreshToken(): Boolean {
        val expiresIn = getAccessTokenExpiresIn()
        val now = System.currentTimeMillis()
        val shouldRefresh = now >= (expiresIn - REFRESH_BEFORE_EXPIRY)
        
        if (shouldRefresh) {
            Log.d(TAG, "토큰 갱신 필요: 만료까지 ${expiresIn - now}ms 남음")
        }
        
        return shouldRefresh
    }

    fun isRefreshTokenExpired(): Boolean {
        val expiresIn = getRefreshTokenExpiresIn()
        val now = System.currentTimeMillis()
        val isExpired = now >= expiresIn
        val remainingTime = expiresIn - now
        
        Log.d(TAG, "리프레시 토큰 만료 확인: ${if (isExpired) "만료됨" else "유효함"}, 남은 시간: ${remainingTime}ms")
        return isExpired
    }

    fun getSharedPreferences(): SharedPreferences = prefs
    
    fun logTokenStatus() {
        val accessToken = getAccessToken()
        val refreshToken = getRefreshToken()
        val accessExpiresAt = getAccessTokenExpiresIn()
        val refreshExpiresAt = getRefreshTokenExpiresIn()
        val now = System.currentTimeMillis()
        
        Log.d(TAG, "토큰 상태:")
        Log.d(TAG, "액세스 토큰: ${if (accessToken != null) "존재함" else "없음"}")
        Log.d(TAG, "리프레시 토큰: ${if (refreshToken != null) "존재함" else "없음"}")
        Log.d(TAG, "액세스 토큰 만료 시간: ${if (accessExpiresAt > 0) "${accessExpiresAt - now}ms 후" else "설정되지 않음"}")
        Log.d(TAG, "리프레시 토큰 만료 시간: ${if (refreshExpiresAt > 0) "${refreshExpiresAt - now}ms 후" else "설정되지 않음"}")
        Log.d(TAG, "액세스 토큰 만료 여부: ${if (now >= accessExpiresAt) "만료됨" else "유효함"}")
        Log.d(TAG, "리프레시 토큰 만료 여부: ${if (now >= refreshExpiresAt) "만료됨" else "유효함"}")
    }
} 