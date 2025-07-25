package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.R
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.foundation.clickable
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.ImeAction
import com.example.chickenstock.ui.theme.Gray300
import com.example.chickenstock.ui.theme.Gray500
import androidx.activity.compose.BackHandler
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.LoginRequest
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.api.TokenExchangeService
import com.example.chickenstock.api.NotificationService
import com.example.chickenstock.api.FcmTokenRequest
import androidx.compose.ui.platform.LocalContext
import com.google.firebase.messaging.FirebaseMessaging
import kotlinx.coroutines.launch
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import android.content.Intent
import android.net.Uri
import android.util.Log
import android.content.Context

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun LoginScreen(
    navController: NavController,
    authViewModel: AuthViewModel
) {
    val context = LocalContext.current
    var email by remember { mutableStateOf("") }
    var password by remember { mutableStateOf("") }
    var isLoading by remember { mutableStateOf(false) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    
    // 경고 대화상자 표시 상태
    var showExitDialog by remember { mutableStateOf(false) }
    var showSuccessDialog by remember { mutableStateOf(false) }
    
    val coroutineScope = rememberCoroutineScope()
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }
    val tokenManager = remember { TokenManager.getInstance(context) }
    val tokenExchangeService = remember { RetrofitClient.getInstance(context).create(TokenExchangeService::class.java) }
    
    // 사용자 입력이 있는지 확인하는 함수
    fun hasUserInput(): Boolean {
        return email.isNotEmpty() || password.isNotEmpty()
    }
    
    // 뒤로가기 처리
    fun handleBackNavigation() {
        if (hasUserInput()) {
            showExitDialog = true
        } else {
            navController.navigateUp()
        }
    }
    
    // 시스템 뒤로가기 처리
    BackHandler {
        handleBackNavigation()
    }
    
    // 소셜 로그인 처리 함수
    fun handleSocialLogin(provider: String) {
        val redirectUri = "chickenstock://oauth2callback"
        val authUrl = "https://chickenstock.shop/api/auth/oauth2/redirect/$provider?redirectUri=$redirectUri&platform=mobile"
        
        Log.d("SocialLogin", "Opening URL: $authUrl")
        
        // 시스템 브라우저로 소셜 로그인 URL 열기
        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(authUrl))
        context.startActivity(intent)
    }
    
    // 경고 대화상자
    if (showExitDialog) {
        AlertDialog(
            onDismissRequest = { showExitDialog = false },
            title = { Text("로그인 취소", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("입력된 정보가 모두 초기화됩니다. 정말 나가시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(
                    onClick = {
                        showExitDialog = false
                        navController.navigateUp()
                    }
                ) {
                    Text("나가기", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showExitDialog = false }
                ) {
                    Text("계속 작성하기", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }
    
    // 성공 다이얼로그
    if (showSuccessDialog) {
        AlertDialog(
            onDismissRequest = { },
            title = { Text("로그인 성공", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("환영합니다!", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(
                    onClick = {
                        showSuccessDialog = false
                        authViewModel.login()
                        // FCM 토큰 등록
                        registerFcmTokenToServer(context)
                        navController.navigate(Screen.Home.route) {
                            popUpTo(navController.graph.startDestinationId) {
                                inclusive = true
                            }
                        }
                    }
                ) {
                    Text("확인", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }
    
    Box(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
    ) {
        // 뒤로가기 버튼
        IconButton(
            onClick = { handleBackNavigation() },
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(16.dp)
        ) {
            Icon(
                imageVector = Icons.Filled.KeyboardArrowLeft,
                contentDescription = "뒤로가기",
                modifier = Modifier.size(32.dp),
                tint = Color.Black
            )
        }
        
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 24.dp)
                .align(Alignment.Center),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            // 로고와 제목
            Row(
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier.padding(bottom = 32.dp)
            ) {
                Text(
                    text = "치킨\n스톡",
                    fontSize = 32.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily,
                    lineHeight = 36.sp,
                    textAlign = TextAlign.End
                )
                Spacer(modifier = Modifier.width(16.dp))
                Icon(
                    painter = painterResource(id = R.drawable.logo),
                    contentDescription = "로고",
                    tint = Color.Unspecified,
                    modifier = Modifier.size(80.dp)
                )
            }
            
            // 이메일 입력 필드
            OutlinedTextField(
                value = email,
                onValueChange = { 
                    email = it
                    errorMessage = null
                },
                label = { Text("이메일", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (errorMessage != null) 4.dp else 12.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    errorBorderColor = Color.Red,
                    errorContainerColor = Color.White,
                    errorLabelColor = Color.Red
                ),
                singleLine = true,
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Email,
                    imeAction = ImeAction.Next
                ),
                isError = errorMessage != null
            )
            
            // 비밀번호 입력 필드
            OutlinedTextField(
                value = password,
                onValueChange = { 
                    password = it
                    errorMessage = null
                },
                label = { Text("비밀번호", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (errorMessage != null) 4.dp else 24.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    errorBorderColor = Color.Red,
                    errorContainerColor = Color.White,
                    errorLabelColor = Color.Red
                ),
                visualTransformation = PasswordVisualTransformation(),
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Password,
                    imeAction = ImeAction.Done
                ),
                singleLine = true,
                isError = errorMessage != null
            )

            // 에러 메시지
            if (errorMessage != null) {
                Text(
                    text = errorMessage ?: "",
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
            }
            
            // 로그인 버튼
            Button(
                onClick = { 
                    if (email.isEmpty() || password.isEmpty()) {
                        errorMessage = "이메일과 비밀번호를 모두 입력해주세요"
                        return@Button
                    }

                    coroutineScope.launch {
                        isLoading = true
                        errorMessage = null
                        try {
                            val loginRequest = LoginRequest(email, password)
                            println("로그인 시도: $email") // 디버그 로그
                            
                            val response = authService.login(loginRequest)
                            println("로그인 응답: ${response.code()}") // 디버그 로그
                            
                            when {
                                response.isSuccessful -> {
                                    response.body()?.let { loginResponse ->
                                        println("로그인 성공: 토큰 수신") // 디버그 로그
                                        println("Access Token: ${loginResponse.accessToken}")
                                        println("Refresh Token: ${loginResponse.refreshToken}")
                                        println("Access Token 만료 시간: ${loginResponse.accessTokenExpiresIn}")
                                        
                                        tokenManager.saveTokens(
                                            loginResponse.accessToken,
                                            loginResponse.refreshToken,
                                            loginResponse.accessTokenExpiresIn
                                        )
                                        
                                        // 토큰 저장 후 확인
                                        println("저장된 Access Token: ${tokenManager.getAccessToken()}")
                                        println("저장된 Refresh Token: ${tokenManager.getRefreshToken()}")
                                        println("저장된 만료 시간: ${tokenManager.getAccessTokenExpiresIn()}")
                                        
                                        // FCM 토큰 등록
                                        registerFcmTokenToServer(context)
                                        
                                        showSuccessDialog = true
                                    } ?: run {
                                        errorMessage = "서버 응답이 올바르지 않습니다"
                                        println("로그인 실패: 응답 본문 없음") // 디버그 로그
                                    }
                                }
                                response.code() == 401 -> {
                                    errorMessage = "이메일 또는 비밀번호가 일치하지 않습니다"
                                    println("로그인 실패: 인증 실패 (401)") // 디버그 로그
                                }
                                response.code() == 404 -> {
                                    errorMessage = "등록되지 않은 사용자입니다"
                                    println("로그인 실패: 사용자 없음 (404)") // 디버그 로그
                                }
                                else -> {
                                    errorMessage = "로그인에 실패했습니다 (${response.code()})"
                                    println("로그인 실패: ${response.code()}") // 디버그 로그
                                }
                            }
                        } catch (e: Exception) {
                            println("로그인 오류: ${e.message}") // 디버그 로그
                            errorMessage = when {
                                e.message?.contains("Unable to resolve host") == true -> "서버에 연결할 수 없습니다. 인터넷 연결을 확인해주세요."
                                e.message?.contains("timeout") == true -> "서버 응답이 지연되고 있습니다. 잠시 후 다시 시도해주세요."
                                else -> "네트워크 오류가 발생했습니다. 잠시 후 다시 시도해주세요."
                            }
                        } finally {
                            isLoading = false
                        }
                    }
                },
                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = RoundedCornerShape(12.dp),
                contentPadding = PaddingValues(vertical = 12.dp),
                enabled = !isLoading
            ) {
                if (isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(20.dp),
                        color = Color.Black,
                        strokeWidth = 2.dp
                    )
                } else {
                    Text(
                        "로그인",
                        color = Color.Black,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily,
                        fontSize = 18.sp
                    )
                }
            }
            
            // 회원가입 및 비밀번호 찾기 링크
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(top = 16.dp),
                horizontalArrangement = Arrangement.Center
            ) {
                Text(
                    "회원가입",
                    color = Gray500,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 14.sp,
                    modifier = Modifier.clickable { navController.navigate("terms_agreement") }
                )
                Text(
                    " | ",
                    color = Gray500,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 14.sp
                )
                Text(
                    "비밀번호 찾기",
                    color = Gray500,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 14.sp,
                    modifier = Modifier.clickable { navController.navigate(Screen.FindPW.route) }
                )
            }
            
            // 소셜 로그인 구분선
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(vertical = 32.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                Divider(
                    modifier = Modifier.weight(1f),
                    color = Color.LightGray
                )
                Text(
                    "소셜",
                    color = Gray500,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 14.sp,
                    modifier = Modifier.padding(horizontal = 16.dp)
                )
                Divider(
                    modifier = Modifier.weight(1f),
                    color = Color.LightGray
                )
            }
            
            // 소셜 로그인 버튼
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceEvenly
            ) {
                // 구글 로그인
                IconButton(
                    onClick = { handleSocialLogin("google") },
                    modifier = Modifier
                        .size(48.dp)
                        .clip(RoundedCornerShape(24.dp))
                ) {
                    Icon(
                        painter = painterResource(id = R.drawable.google_logo),
                        contentDescription = "구글 로그인",
                        tint = Color.Unspecified,
                        modifier = Modifier.size(48.dp)
                    )
                }
                
                // 카카오 로그인
                IconButton(
                    onClick = { handleSocialLogin("kakao") },
                    modifier = Modifier
                        .size(48.dp)
                        .clip(RoundedCornerShape(24.dp))
                ) {
                    Icon(
                        painter = painterResource(id = R.drawable.kakao_logo),
                        contentDescription = "카카오 로그인",
                        tint = Color.Unspecified,
                        modifier = Modifier.size(48.dp)
                    )
                }
                
                // 네이버 로그인
                IconButton(
                    onClick = { handleSocialLogin("naver") },
                    modifier = Modifier
                        .size(48.dp)
                        .clip(RoundedCornerShape(24.dp))
                ) {
                    Icon(
                        painter = painterResource(id = R.drawable.naver_logo),
                        contentDescription = "네이버 로그인",
                        tint = Color.Unspecified,
                        modifier = Modifier.size(48.dp)
                    )
                }
            }
        }
    }
}

fun registerFcmTokenToServer(context: Context, onResult: ((Boolean, String) -> Unit)? = null) {
    FirebaseMessaging.getInstance().token.addOnSuccessListener { fcmToken: String ->
        val accessToken = TokenManager.getInstance(context).getAccessToken()
        Log.d("FCM", "registerFcmTokenToServer called. fcmToken=$fcmToken, accessToken=$accessToken")
        if (!accessToken.isNullOrBlank()) {
            val service = RetrofitClient.getInstance(context).create(NotificationService::class.java)
            CoroutineScope(Dispatchers.IO).launch {
                try {
                    val response = service.registerFcmToken("Bearer $accessToken", FcmTokenRequest(fcmToken))
                    val msg = "registerFcmTokenToServer response: ${response.code()} ${response.body()?.message ?: response.errorBody()?.string()}"
                    Log.d("FCM", msg)
                    onResult?.invoke(response.isSuccessful, msg)
                } catch (e: Exception) {
                    Log.e("FCM", "registerFcmTokenToServer error: ${e.message}")
                    onResult?.invoke(false, e.message ?: "Unknown error")
                }
            }
        } else {
            onResult?.invoke(false, "No access token")
        }
    }
} 