package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.ImeAction
import com.example.chickenstock.ui.theme.Gray300
import androidx.activity.compose.BackHandler
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.VerifyCodeRequest
import com.example.chickenstock.api.EmailCheckRequest
import kotlinx.coroutines.launch
import androidx.compose.runtime.rememberCoroutineScope
import com.example.chickenstock.viewmodel.MainViewModel
import androidx.compose.ui.platform.LocalContext
import com.example.chickenstock.api.SignupRequest

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun VerificationScreen(
    navController: NavController,
    email: String,
    name: String,
    nickname: String,
    password: String,
    viewModel: MainViewModel
) {
    // 이메일 디코딩
    val decodedEmail = remember(email) { email.replace("__AT__", "@") }

    // 하단바와 탑바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setBottomBarVisibility(false)
        viewModel.setTopBarVisibility(false)
    }

    var verificationCode by remember { mutableStateOf("") }
    var isError by remember { mutableStateOf(false) }
    var resendSuccess by remember { mutableStateOf<Boolean?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }

    Box(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
    ) {
        // 뒤로가기 버튼
        IconButton(
            onClick = { navController.navigateUp() },
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
                .fillMaxSize()
                .padding(24.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
            verticalArrangement = Arrangement.Center
        ) {
            Text(
                text = "인증번호 입력",
                fontSize = 24.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.padding(bottom = 32.dp)
            )

            Text(
                text = "입력하신 이메일로 인증번호가 발송되었습니다",
                fontSize = 16.sp,
                fontFamily = SCDreamFontFamily,
                color = Color.Gray,
                modifier = Modifier.padding(bottom = 24.dp)
            )

            OutlinedTextField(
                value = verificationCode,
                onValueChange = { 
                    verificationCode = it
                    isError = false
                },
                label = { Text("인증번호", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (isError) 4.dp else 24.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (isError) Color.Red else Color.Gray,
                    focusedBorderColor = if (isError) Color.Red else Color.Gray,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                isError = isError,
                singleLine = true,
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Number,
                    imeAction = ImeAction.Done
                )
            )

            if (isError && errorMessage != null) {
                Text(
                    text = errorMessage!!,
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, top = 8.dp)
                )
            }

            // 재전송 상태 메시지
            when (resendSuccess) {
                true -> Text(
                    text = "인증번호가 재전송되었습니다",
                    color = Color(0xFF4CAF50),
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
                false -> Text(
                    text = "인증번호 재전송에 실패했습니다",
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
                null -> {}
            }

            Button(
                onClick = {
                    coroutineScope.launch {
                        try {
                            // 인증번호 확인
                            val verifyResponse = authService.verifyCode(VerifyCodeRequest(decodedEmail, verificationCode))
                            if (verifyResponse.isSuccessful && verifyResponse.body()?.success == true) {
                                // 인증 성공 시 회원가입 API 호출
                                try {
                                    println("회원가입 시도: email=$decodedEmail, nickname=$nickname, name=$name") // 디버그 로그
                                    val signupResponse = authService.signup(
                                        SignupRequest(
                                            email = decodedEmail,
                                            password = password,
                                            nickname = nickname,
                                            name = name
                                        )
                                    )
                                    println("회원가입 응답 코드: ${signupResponse.code()}") // 디버그 로그
                                    if (signupResponse.isSuccessful) {
                                        // 회원가입 성공 시 성공 화면으로 이동
                                        navController.navigate("signup_success") {
                                            popUpTo("signup") { inclusive = true }
                                        }
                                    } else {
                                        // 회원가입 실패
                                        println("회원가입 실패 응답: ${signupResponse.errorBody()?.string()}") // 디버그 로그
                                        isError = true
                                        errorMessage = "회원가입에 실패했습니다. 다시 시도해주세요."
                                    }
                                } catch (e: Exception) {
                                    println("회원가입 예외 발생: ${e.message}") // 디버그 로그
                                    isError = true
                                    errorMessage = "회원가입 중 오류가 발생했습니다. 다시 시도해주세요."
                                }
                            } else {
                                isError = true
                                errorMessage = "인증번호가 일치하지 않습니다."
                            }
                        } catch (e: Exception) {
                            isError = true
                            errorMessage = "네트워크 오류가 발생했습니다. 다시 시도해주세요."
                        }
                    }
                },
                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = RoundedCornerShape(12.dp)
            ) {
                Text(
                    "확인",
                    color = Color.Black,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 18.sp
                )
            }

            // 인증번호 재전송 버튼
            TextButton(
                onClick = {
                    coroutineScope.launch {
                        isLoading = true
                        try {
                            val response = authService.sendVerificationCode(EmailCheckRequest(decodedEmail))
                            resendSuccess = response.isSuccessful
                            // 3초 후에 메시지 숨기기
                            kotlinx.coroutines.delay(3000)
                            resendSuccess = null
                        } catch (e: Exception) {
                            resendSuccess = false
                            // 3초 후에 메시지 숨기기
                            kotlinx.coroutines.delay(3000)
                            resendSuccess = null
                        } finally {
                            isLoading = false
                        }
                    }
                },
                modifier = Modifier.padding(top = 8.dp),
                enabled = !isLoading
            ) {
                if (isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(16.dp),
                        color = Color(0xFF0066CC),
                        strokeWidth = 2.dp
                    )
                } else {
                    Text(
                        "인증번호 재전송",
                        color = Color(0xFF0066CC),
                        fontFamily = SCDreamFontFamily
                    )
                }
            }
        }
    }
} 