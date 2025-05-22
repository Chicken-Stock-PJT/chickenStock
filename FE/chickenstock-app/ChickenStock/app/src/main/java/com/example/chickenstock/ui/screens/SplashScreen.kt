package com.example.chickenstock.ui.screens

import android.util.Log
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import com.example.chickenstock.R
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.viewmodel.MainViewModel
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking

// 앱 테마 색상
private val Yellow1 = Color(0xFFFFEB3B)
private const val TAG = "SplashScreen"
private const val SPLASH_DELAY = 3000L // 3초

@Composable
fun SplashScreen(
    navController: NavController,
    authViewModel: AuthViewModel,
    viewModel: MainViewModel
) {
    val context = LocalContext.current
    var canNavigate by remember { mutableStateOf(false) }
    var destination by remember { mutableStateOf(Screen.Home.route) }
    
    // splashDone이 true면 바로 홈으로 이동
    if (viewModel.splashDone) {
        LaunchedEffect(Unit) {
            navController.navigate(com.example.chickenstock.navigation.Screen.Home.route) {
                popUpTo(com.example.chickenstock.navigation.Screen.Splash.route) { inclusive = true }
            }
        }
        // UI는 빈 화면 또는 간단한 Progress만
        Box(modifier = Modifier.fillMaxSize())
        return
    }
    
    // 토큰 체크 및 네비게이션 준비
    LaunchedEffect(key1 = navController) {
        // 시작 시간 기록
        val startTime = System.currentTimeMillis()
        Log.d(TAG, "### 스플래시 화면 시작 시간: $startTime ###")
        
        // 토큰 상태 확인
        val isLoggedIn = authViewModel.isLoggedIn.value
        Log.d(TAG, "로그인 상태: $isLoggedIn")
        
        val tokenManager = TokenManager.getInstance(context)
        val accessToken = tokenManager.getAccessToken()
        val refreshToken = tokenManager.getRefreshToken()
        
        if (isLoggedIn) {
            if (!accessToken.isNullOrBlank() && !tokenManager.isAccessTokenExpired()) {
                // 토큰이 유효함 -> 홈으로 이동
                Log.d(TAG, "로그인 상태 + 유효한 토큰 -> 홈으로 이동")
                destination = Screen.Home.route
            } else if (!refreshToken.isNullOrBlank() && !tokenManager.isRefreshTokenExpired()) {
                // 액세스 토큰이 만료됨 -> 무조건 토큰 갱신 시도
                Log.d(TAG, "로그인 상태 + 만료된 토큰 -> 토큰 갱신 시도")
                val success = authViewModel.refreshTokensBlocking()
                if (success) {
                    Log.d(TAG, "토큰 재발급 성공 -> 홈으로 이동")
                    destination = Screen.Home.route
                } else {
                    Log.d(TAG, "토큰 재발급 실패 -> 홈으로 이동")
                    destination = Screen.Home.route
                }
            } else {
                // 리프레시 토큰도 만료됨 -> 로그아웃 처리
                Log.d(TAG, "로그인 상태 + 리프레시 토큰 만료 -> 로그아웃 처리")
                authViewModel.logout()
                destination = Screen.Home.route
            }
        } else {
            // 비로그인 상태 -> 홈으로 이동
            Log.d(TAG, "비로그인 상태 -> 홈으로 이동")
            destination = Screen.Home.route
        }
        
        // 경과 시간 계산
        val elapsedTime = System.currentTimeMillis() - startTime
        val remainingTime = SPLASH_DELAY - elapsedTime
        
        Log.d(TAG, "### 토큰 체크 완료 시간: ${System.currentTimeMillis()} (${elapsedTime}ms 소요) ###")
        Log.d(TAG, "### 남은 대기 시간: ${remainingTime}ms ###")
        
        if (remainingTime > 0) {
            // 직접적인 UI 쓰레드 차단
            Log.d(TAG, "### 스플래시 화면 대기 시작 ###")
            runBlocking {
                delay(remainingTime)
            }
            Log.d(TAG, "### 스플래시 화면 대기 완료 ###")
        }
        
        // 최종 시간 기록
        val totalTime = System.currentTimeMillis() - startTime
        Log.d(TAG, "### 스플래시 화면 총 지속 시간: ${totalTime}ms ###")
        
        // 이제 네비게이션 허용
        canNavigate = true
        viewModel.setSplashDone()
    }
    
    // 스플래시 화면 UI
    Box(
        modifier = Modifier
            .fillMaxSize()
            .background(Color.White),
        contentAlignment = Alignment.Center
    ) {
        Column(
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            Image(
                painter = painterResource(id = R.drawable.logo),
                contentDescription = "Logo",
                modifier = Modifier.size(200.dp)
            )
            CircularProgressIndicator(
                modifier = Modifier
                    .size(48.dp)
                    .padding(top = 16.dp), // 로고와 인디케이터 사이 간격
                color = Yellow1
            )
        }
    }
} 