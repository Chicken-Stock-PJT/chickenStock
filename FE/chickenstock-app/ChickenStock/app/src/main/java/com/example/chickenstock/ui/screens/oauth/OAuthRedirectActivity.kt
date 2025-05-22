package com.example.chickenstock.ui.screens.oauth

import android.content.Intent
import android.os.Bundle
import android.util.Log
import androidx.activity.ComponentActivity
import androidx.lifecycle.lifecycleScope
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.api.TokenExchangeRequest
import com.example.chickenstock.api.TokenExchangeService
import com.example.chickenstock.api.LoginResponse
import com.example.chickenstock.viewmodel.AuthViewModel
import kotlinx.coroutines.launch
import com.example.chickenstock.MainActivity

class OAuthRedirectActivity : ComponentActivity() {
    
    private val TAG = "OAuthRedirectActivity"
    
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        // 딥링크로 전달된 데이터 처리
        intent?.data?.let { uri ->
            Log.d(TAG, "딥링크 수신: $uri")
            
            // oneTimeCode 추출
            val oneTimeCode = uri.getQueryParameter("oneTimeCode")
            Log.d(TAG, "oneTimeCode: $oneTimeCode")
            
            if (oneTimeCode != null) {
                // 토큰 교환 API 호출
                exchangeToken(oneTimeCode)
            } else {
                Log.e(TAG, "oneTimeCode가 없습니다")
                navigateToLogin()
            }
        } ?: run {
            Log.e(TAG, "URI 데이터가 없습니다")
            navigateToLogin()
        }
    }
    
    private fun exchangeToken(oneTimeCode: String) {
        lifecycleScope.launch {
            try {
                val tokenExchangeService = RetrofitClient.getInstance(this@OAuthRedirectActivity)
                    .create(TokenExchangeService::class.java)
                
                val response = tokenExchangeService.exchangeToken(
                    TokenExchangeRequest(oneTimeCode, "mobile")
                )
                
                if (response.isSuccessful) {
                    response.body()?.let { loginResponse ->
                        Log.d(TAG, "토큰 교환 성공")
                        
                        // 토큰 저장
                        val tokenManager = TokenManager.getInstance(this@OAuthRedirectActivity)
                        tokenManager.saveTokens(
                            loginResponse.accessToken,
                            loginResponse.refreshToken,
                            loginResponse.accessTokenExpiresIn
                        )
                        
                        // 소셜 로그인 여부 저장
                        val prefs = getSharedPreferences("auth_prefs", MODE_PRIVATE)
                        prefs.edit().putBoolean("is_social_login", true).apply()
                        
                        // FCM 토큰 등록 (응답 로그도 직접 출력)
                        com.example.chickenstock.ui.screens.login.registerFcmTokenToServer(this@OAuthRedirectActivity) { success, msg ->
                            Log.d(TAG, "소셜 로그인 후 FCM 등록 응답: $success, $msg")
                        }
                        Log.d(TAG, "소셜 로그인 후 FCM 토큰 등록 요청 완료")
                        
                        // 로그인 상태 업데이트
                        AuthViewModel.Factory(this@OAuthRedirectActivity).create(AuthViewModel::class.java).login()
                        
                        // 홈 화면으로 이동
                        navigateToHome()
                    } ?: run {
                        Log.e(TAG, "토큰 응답이 비어 있습니다")
                        navigateToLogin()
                    }
                } else {
                    Log.e(TAG, "토큰 교환 실패: ${response.code()}")
                    val errorBody = response.errorBody()?.string()
                    Log.e(TAG, "에러 응답: $errorBody")
                    navigateToLogin()
                }
            } catch (e: Exception) {
                Log.e(TAG, "토큰 교환 오류: ${e.message}", e)
                navigateToLogin()
            }
        }
    }
    
    private fun navigateToHome() {
        val intent = Intent(this, MainActivity::class.java).apply {
            flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK
            putExtra("destination", Screen.Home.route)
        }
        startActivity(intent)
        finish()
    }
    
    private fun navigateToLogin() {
        val intent = Intent(this, MainActivity::class.java).apply {
            flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TASK
            putExtra("destination", Screen.Login.route)
        }
        startActivity(intent)
        finish()
    }
} 