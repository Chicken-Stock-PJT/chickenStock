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
import java.util.concurrent.atomic.AtomicBoolean

class AuthViewModel(private val context: Context) : ViewModel() {
    private val _isLoggedIn = mutableStateOf(false)
    val isLoggedIn: State<Boolean> = _isLoggedIn

    private val _token = MutableStateFlow<String?>(null)
    private val tokenManager = TokenManager.getInstance(context)
    
    private val TAG = "AuthViewModel"

    private val _isRefreshing = MutableStateFlow(false)
    val isRefreshing: StateFlow<Boolean> = _isRefreshing

    private val _isRefreshingToken = AtomicBoolean(false)
    val isRefreshingToken: Boolean get() = _isRefreshingToken.get()

    fun getToken(): String? = _token.value

    init {
        // 앱 시작 시 저장된 로그인 상태 불러오기
        loadLoginState()
        // 토큰 상태 로깅
        tokenManager.logTokenStatus()
        // 토큰 상태 확인 및 필요시 재발급 (비동기로 실행)
        // viewModelScope.launch {
        // checkAndRefreshTokens()
        // }
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
        if (_isRefreshingToken.get()) {
            Log.d("AuthViewModel", "이미 토큰 갱신 중이므로 스킵")
            return
        }
        val tokenManager = TokenManager.getInstance(context)
        val isTokenExpired = tokenManager.isAccessTokenExpired()
        val needsRefresh = tokenManager.needsTokenRefresh()
        if (isTokenExpired || needsRefresh) {
            Log.d("AuthViewModel", "토큰 재발급 필요")
            refreshTokens()
        }
    }
    
    private fun refreshTokens() {
        if (!_isRefreshingToken.compareAndSet(false, true)) {
            Log.d("AuthViewModel", "[차단:코루틴] 이미 토큰 갱신 중이므로 네트워크 요청도 하지 않음")
            return
        }
        Log.d("AuthViewModel", "토큰 재발급 API 호출")
        try {
            val tokenManager = TokenManager.getInstance(context)
            val refreshToken = tokenManager.getRefreshToken()
            if (refreshToken == null) {
                Log.e("AuthViewModel", "리프레시 토큰이 없음")
                _isRefreshingToken.set(false)
                return
            }
            val authService = RetrofitClient.getInstance(context, ignoreAuthCheck = true).create(AuthService::class.java)
            val requestBody = TokenRefreshRequest(refreshToken)
            viewModelScope.launch {
                try {
                    val response = authService.refreshAccessToken(requestBody)
                    if (response.isSuccessful) {
                        response.body()?.let { tokenResponse ->
                            tokenManager.saveAccessToken(tokenResponse.accessToken)
                            tokenManager.saveRefreshToken(tokenResponse.refreshToken)
                            tokenManager.setAccessTokenExpiry(tokenResponse.accessTokenExpiresIn)
                            tokenManager.setRefreshTokenExpiry(tokenResponse.accessTokenExpiresIn + 2592000000)
                            Log.d("AuthViewModel", "토큰 재발급 성공")
                        }
                    } else {
                        Log.e("AuthViewModel", "토큰 재발급 실패: ${response.code()}")
                        if (response.code() == 401 || response.code() == 403) {
                            // logout() // ← 주석 처리: 토큰 삭제 및 로그아웃 방지
                        }
                    }
                } finally {
                    _isRefreshingToken.set(false)
                }
            }
        } catch (e: Exception) {
            Log.e("AuthViewModel", "토큰 재발급 초기화 중 오류", e)
            _isRefreshingToken.set(false)
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

    // 스플래시 화면에서 사용할 블로킹 방식의 토큰 재발급 함수
    suspend fun refreshTokensBlocking(): Boolean {
        if (_isRefreshingToken.get()) {
            Log.d("AuthViewModel", "[차단:블로킹] 이미 토큰 갱신 중이므로 네트워크 요청도 하지 않음")
            return false
        }
        Log.d("AuthViewModel", "블로킹 방식으로 토큰 재발급 시도")
        _isRefreshingToken.set(true)
        return try {
            val refreshToken = TokenManager.getInstance(context).getRefreshToken() ?: return false
            val authService = RetrofitClient.getInstance(context, ignoreAuthCheck = true).create(AuthService::class.java)
            val requestBody = TokenRefreshRequest(refreshToken)
            val response = authService.refreshAccessToken(requestBody)
            if (response.isSuccessful) {
                response.body()?.let { tokenResponse ->
                    val tokenManager = TokenManager.getInstance(context)
                    tokenManager.saveAccessToken(tokenResponse.accessToken)
                    tokenManager.saveRefreshToken(tokenResponse.refreshToken)
                    tokenManager.setAccessTokenExpiry(tokenResponse.accessTokenExpiresIn)
                    tokenManager.setRefreshTokenExpiry(tokenResponse.accessTokenExpiresIn + 2592000000)
                    Log.d("AuthViewModel", "토큰 재발급 성공")
                    _isRefreshingToken.set(false)
                    return true
                }
            }
            Log.e("AuthViewModel", "토큰 재발급 실패: ${response.code()}")
            _isRefreshingToken.set(false)
            false
        } catch (e: Exception) {
            Log.e("AuthViewModel", "토큰 재발급 중 오류 발생", e)
            _isRefreshingToken.set(false)
            false
        }
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