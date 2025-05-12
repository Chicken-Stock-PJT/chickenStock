package com.example.chickenstock.viewmodel

import android.content.Context
import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.TokenRefreshRequest
import android.util.Log
import kotlinx.coroutines.delay

class AuthViewModel(private val context: Context) : ViewModel() {
    private val _isLoggedIn = mutableStateOf(false)
    val isLoggedIn: State<Boolean> = _isLoggedIn

    private val _token = MutableStateFlow<String?>(null)
    private val tokenManager = TokenManager.getInstance(context)
    
    private val TAG = "AuthViewModel"

    fun getToken(): String? = _token.value

    init {
        // 앱 시작 시 저장된 로그인 상태 불러오기
        loadLoginState()
        // 토큰 상태 로깅
        tokenManager.logTokenStatus()
        // 토큰 상태 확인 및 필요시 재발급
        checkAndRefreshTokens()
    }

    private fun loadLoginState() {
        val sharedPreferences = context.getSharedPreferences("auth_prefs", Context.MODE_PRIVATE)
        val wasLoggedIn = sharedPreferences.getBoolean("is_logged_in", false)
        _isLoggedIn.value = wasLoggedIn
        
        Log.d(TAG, "로그인 상태 로드: $wasLoggedIn")
        
        // 토큰 확인
        val accessToken = tokenManager.getAccessToken()
        val refreshToken = tokenManager.getRefreshToken()
        
        if (wasLoggedIn && (accessToken == null || refreshToken == null)) {
            Log.e(TAG, "로그인 상태지만 토큰이 없습니다. 로그아웃 처리합니다.")
            logout()
        }
    }

    private fun saveLoginState(isLoggedIn: Boolean) {
        val sharedPreferences = context.getSharedPreferences("auth_prefs", Context.MODE_PRIVATE)
        sharedPreferences.edit().putBoolean("is_logged_in", isLoggedIn).apply()
        Log.d(TAG, "로그인 상태 저장: $isLoggedIn")
    }

    private fun checkAndRefreshTokens() {
        if (_isLoggedIn.value) {
            val accessToken = tokenManager.getAccessToken()
            val refreshToken = tokenManager.getRefreshToken()
            val isExpired = tokenManager.isTokenExpired()
            val shouldRefresh = tokenManager.shouldRefreshToken()
            
            Log.d(TAG, "토큰 체크: 액세스 토큰=${accessToken != null}, 리프레시 토큰=${refreshToken != null}, 만료됨=$isExpired, 갱신필요=$shouldRefresh")

            if (accessToken != null && refreshToken != null && (isExpired || shouldRefresh)) {
                Log.d(TAG, "토큰 재발급 시도")
                refreshTokens(accessToken, refreshToken)
            } else if (accessToken == null || refreshToken == null) {
                Log.e(TAG, "토큰이 없어서 로그아웃 처리합니다.")
                logout()
            }
        }
    }
    
    private fun refreshTokens(accessToken: String, refreshToken: String) {
        viewModelScope.launch {
            try {
                Log.d(TAG, "토큰 재발급 API 호출")
                val authService = RetrofitClient.getInstance(context).create(AuthService::class.java)
                val response = authService.refreshAllTokens(
                    TokenRefreshRequest(accessToken, refreshToken)
                )

                if (response.isSuccessful && response.body() != null) {
                    val newTokens = response.body()!!
                    Log.d(TAG, "토큰 재발급 성공")
                    tokenManager.saveTokens(
                        newTokens.accessToken,
                        newTokens.refreshToken,
                        newTokens.accessTokenExpiresIn - System.currentTimeMillis(), // 서버에서 받은 만료 시간 사용
                        30 * 24 * 60 * 60 * 1000L // 리프레시 토큰 30일
                    )
                } else {
                    Log.e(TAG, "토큰 재발급 실패: ${response.code()}")
                    // 재발급 실패 시 로그아웃
                    logout()
                }
            } catch (e: Exception) {
                Log.e(TAG, "토큰 재발급 중 오류 발생: ${e.message}", e)
                // 에러 발생 시 최대 3번까지 재시도
                retryRefreshToken(accessToken, refreshToken, 1)
            }
        }
    }
    
    private fun retryRefreshToken(accessToken: String, refreshToken: String, attempt: Int) {
        if (attempt > 3) {
            Log.e(TAG, "최대 재시도 횟수 초과, 로그아웃 처리합니다.")
            logout()
            return
        }
        
        viewModelScope.launch {
            try {
                // 점점 더 긴 시간 대기
                val delayTime = attempt * 1000L
                Log.d(TAG, "토큰 재발급 재시도 ($attempt/3) - ${delayTime}ms 후")
                delay(delayTime)
                
                val authService = RetrofitClient.getInstance(context).create(AuthService::class.java)
                val response = authService.refreshAllTokens(
                    TokenRefreshRequest(accessToken, refreshToken)
                )
                
                if (response.isSuccessful && response.body() != null) {
                    val newTokens = response.body()!!
                    Log.d(TAG, "토큰 재발급 성공 (재시도: $attempt)")
                    tokenManager.saveTokens(
                        newTokens.accessToken,
                        newTokens.refreshToken,
                        newTokens.accessTokenExpiresIn - System.currentTimeMillis(), // 서버에서 받은 만료 시간 사용
                        30 * 24 * 60 * 60 * 1000L // 리프레시 토큰 30일
                    )
                } else {
                    Log.e(TAG, "토큰 재발급 실패 (재시도: $attempt): ${response.code()}")
                    retryRefreshToken(accessToken, refreshToken, attempt + 1)
                }
            } catch (e: Exception) {
                Log.e(TAG, "토큰 재발급 중 오류 발생 (재시도: $attempt): ${e.message}", e)
                retryRefreshToken(accessToken, refreshToken, attempt + 1)
            }
        }
    }

    fun login() {
        _isLoggedIn.value = true
        // 로그인 상태 저장
        saveLoginState(true)
    }

    fun logout() {
        Log.d(TAG, "로그아웃 처리")
        _isLoggedIn.value = false
        _token.value = null
        // 토큰 삭제
        tokenManager.clearTokens()
        // 로그인 상태 저장
        saveLoginState(false)
    }

    /**
     * 팩토리 클래스
     */
    class Factory(private val context: Context) : ViewModelProvider.Factory {
        override fun <T : ViewModel> create(modelClass: Class<T>): T {
            if (modelClass.isAssignableFrom(AuthViewModel::class.java)) {
                @Suppress("UNCHECKED_CAST")
                return AuthViewModel(context) as T
            }
            throw IllegalArgumentException("Unknown ViewModel class")
        }
    }
} 